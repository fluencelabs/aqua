package aqua.semantics

import aqua.raw.ops.{FuncOp, SeqGroupTag}
import aqua.raw.{Raw, RawContext, RawPart}
import aqua.parser.lexer.Token
import aqua.parser.{Ast, Expr}
import aqua.semantics.rules.abilities.{AbilitiesAlgebra, AbilitiesInterpreter, AbilitiesState}
import aqua.semantics.rules.names.{NamesAlgebra, NamesInterpreter, NamesState}
import aqua.semantics.rules.types.{TypesAlgebra, TypesInterpreter, TypesState}
import aqua.semantics.rules.{ReportError, ValuesAlgebra}
import cats.arrow.FunctionK
import cats.data.*
import cats.data.Validated.{Invalid, Valid}
import cats.kernel.Monoid
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.semigroup.*
import cats.{Eval, Monad, Semigroup}
import monocle.Lens
import monocle.macros.GenLens
import scribe.{log, Logging}

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
  // TODO: pass external token definitions for the RawContext somehow
  def interpret[S[_]](ast: Ast[S], init: RawContext): Eval[(CompilerState[S], RawContext)] =
    astToState[S](ast)
      .run(CompilerState.init[S](init))
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

  // TODO: return just RawContext on the right side
  def process[S[_]](
    ast: Ast[S],
    init: RawContext
  ): ValidatedNec[SemanticError[S], (CompilerState[S], RawContext)] =
    interpret(ast, init).map { case (state, ctx) =>
      NonEmptyChain
        .fromChain(state.errors)
        .fold[ValidatedNec[SemanticError[S], (CompilerState[S], RawContext)]](
          Valid(state -> ctx)
        )(Invalid(_))
    }
      // TODO: return as Eval
      .value
}
