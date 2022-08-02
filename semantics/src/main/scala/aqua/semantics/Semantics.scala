package aqua.semantics

import aqua.parser.head.{HeadExpr, HeaderExpr, ImportExpr, ImportFromExpr}
import aqua.parser.lexer.{LiteralToken, Token}
import aqua.parser.{Ast, Expr}
import aqua.raw.ops.{FuncOp, SeqGroupTag}
import aqua.raw.{Raw, RawContext, RawPart}
import aqua.semantics.header.Picker
import aqua.semantics.header.Picker.*
import aqua.semantics.lsp.{LspContext, TokenDef, TokenInfo, TokenType}
import aqua.semantics.rules.abilities.{AbilitiesAlgebra, AbilitiesInterpreter, AbilitiesState}
import aqua.semantics.rules.names.{NamesAlgebra, NamesInterpreter, NamesState}
import aqua.semantics.rules.types.{TypesAlgebra, TypesInterpreter, TypesState}
import aqua.semantics.rules.{ReportError, ValuesAlgebra}
import cats.arrow.FunctionK
import cats.data.*
import cats.Reducible
import cats.data.Validated.{Invalid, Valid}
import cats.kernel.Monoid
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.foldable.*
import cats.syntax.reducible.*
import cats.free.CofreeInstances
import cats.syntax.semigroup.*
import cats.{Eval, Monad, Semigroup}
import monocle.Lens
import monocle.macros.GenLens
import scribe.{Logging, log}
import cats.free.Cofree

sealed trait Semantics[S[_], C] {

  def process(
    ast: Ast[S],
    init: C
  ): ValidatedNec[SemanticError[S], C]
}

class RawSemantics[S[_]](implicit p: Picker[RawContext]) extends Semantics[S, RawContext] {

  def process(
    ast: Ast[S],
    init: RawContext
  ): ValidatedNec[SemanticError[S], RawContext] =
    Semantics
      .interpret(ast, CompilerState.init(init), init)
      .map { case (state, ctx) =>
        NonEmptyChain
          .fromChain(state.errors)
          .fold[ValidatedNec[SemanticError[S], RawContext]](
            Valid(ctx)
          )(Invalid(_))
      }
      // TODO: return as Eval
      .value
}

class LspSemantics[S[_]] extends Semantics[S, LspContext[S]] {

  def getImportTokens(ast: Ast[S]): List[LiteralToken[S]] = {
    ast.head.foldLeft[List[LiteralToken[S]]](Nil){ case (l, header) =>
      header match {
        case ImportExpr(fn) =>
          l :+ fn
        case ImportFromExpr(_, fn) => l :+ fn
        case _ => l
      }
    }
  }

  def process(
    ast: Ast[S],
    init: LspContext[S]
  ): ValidatedNec[SemanticError[S], LspContext[S]] = {

    val rawState = CompilerState.init[S](init.raw)
    val initState = rawState.copy(
      names = rawState.names.copy(
        rootArrows = rawState.names.rootArrows ++ init.rootArrows,
        constants = rawState.names.constants ++ init.constants
      ),
      abilities = rawState.abilities.copy(
        definitions = rawState.abilities.definitions ++ init.abDefinitions
      )
    )

    val importTokens = getImportTokens(ast)


    Semantics
      .interpret(ast, initState, init.raw)
      .map { case (state, ctx) =>
        NonEmptyChain
          .fromChain(state.errors)
          .fold[ValidatedNec[SemanticError[S], LspContext[S]]] {
            Valid(
              LspContext(
                raw = ctx,
                rootArrows = state.names.rootArrows,
                constants = state.names.constants,
                abDefinitions = state.abilities.definitions,
                locations = state.locations,
                importTokens = importTokens
              )
            )
          }(Invalid(_))
      }
      // TODO: return as Eval
      .value
  }
}

object Semantics extends Logging {

  private def folder[S[_], G[_]: Monad](implicit
    A: AbilitiesAlgebra[S, G],
    N: NamesAlgebra[S, G],
    T: TypesAlgebra[S, G]
  ): (Expr[S], Chain[G[Raw]]) => Eval[G[Raw]] = { case (expr, inners) =>
    Eval later ExprSem
      .getProg[S, G](expr)
      .apply(
        // TODO instead of foldRight, do slidingWindow for 2 elements, merge right associative ones
        // Then foldLeft just like now
        inners
          .foldRight[G[List[Raw]]](List.empty[Raw].pure[G]) { case (a, b) =>
            (a, b).mapN {
              case (prev: FuncOp, (next: FuncOp) :: tail) if next.isRightAssoc =>
                (prev :+: next) :: tail
              case (prev, acc) => prev :: acc
            }
          }
          .map(
            _.reduceLeftOption(_ |+| _)
              .getOrElse(Raw.empty("AST is empty"))
          )
      )
  }

  type Interpreter[S[_], A] = State[CompilerState[S], A]

  def transpile[S[_]](ast: Ast[S]): Interpreter[S, Raw] = {
    import monocle.syntax.all.*

    implicit val re: ReportError[S, CompilerState[S]] =
      (st: CompilerState[S], token: Token[S], hints: List[String]) =>
        st.focus(_.errors).modify(_.append(RulesViolated(token, hints)))

    implicit val ns: Lens[CompilerState[S], NamesState[S]] = GenLens[CompilerState[S]](_.names)

    implicit val as: Lens[CompilerState[S], AbilitiesState[S]] =
      GenLens[CompilerState[S]](_.abilities)

    implicit val ts: Lens[CompilerState[S], TypesState[S]] = GenLens[CompilerState[S]](_.types)

    implicit val typesInterpreter: TypesInterpreter[S, CompilerState[S]] =
      new TypesInterpreter[S, CompilerState[S]]
    implicit val abilitiesInterpreter: AbilitiesInterpreter[S, CompilerState[S]] =
      new AbilitiesInterpreter[S, CompilerState[S]]
    implicit val namesInterpreter: NamesInterpreter[S, CompilerState[S]] =
      new NamesInterpreter[S, CompilerState[S]]
    ast.cata(folder[S, Interpreter[S, *]]).value
  }

  private def astToState[S[_]](ast: Ast[S]): Interpreter[S, Raw] =
    transpile[S](ast)

  // If there are any errors, they're inside CompilerState[S]
  def interpret[S[_]](
    ast: Ast[S],
    initState: CompilerState[S],
    init: RawContext
  ): Eval[(CompilerState[S], RawContext)] =
    astToState[S](ast)
      .run(initState)
      .map {
        case (state, _: Raw.Empty) =>
          // No `parts`, but has `init`
          (
            state,
            RawContext.blank.copy(
              init = Some(init.copy(module = init.module.map(_ + "|init")))
                .filter(_ != RawContext.blank)
            )
          )

        case (state, part: (RawPart | RawPart.Parts)) =>
          state -> RawPart
            .contextPart(part)
            .parts
            .foldLeft(
              RawContext.blank.copy(
                init = Some(init.copy(module = init.module.map(_ + "|init")))
                  .filter(_ != RawContext.blank)
              )
            ) { case (ctx, p) =>
              ctx.copy(parts = ctx.parts :+ (ctx -> p))
            }
        case (state: CompilerState[S], m) =>
          logger.error("Got unexpected " + m)
          state.copy(errors = state.errors :+ WrongAST(ast)) -> RawContext.blank.copy(
            init = Some(init.copy(module = init.module.map(_ + "|init")))
              .filter(_ != RawContext.blank)
          )
      }
}
