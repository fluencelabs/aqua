package aqua.semantics

import aqua.raw.ops.FuncOp
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
import cats.{Eval, Monad}
import monocle.Lens
import monocle.macros.GenLens
import scribe.Logging

object Semantics extends Logging {

  def folder[S[_], G[_]: Monad](implicit
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
          .map(_.reduceLeftOption(_ |+| _).getOrElse(Raw.empty("AST is empty")))
      )
  }

  type Interpreter[S[_], A] = State[CompilerState[S], A]

  // todo: is it needed?
  type Alg[S[_]] = TypesAlgebra[S, Interpreter[S, *]]
    with NamesAlgebra[S, Interpreter[S, *]] with ValuesAlgebra[S, Interpreter[S, *]]
    with AbilitiesAlgebra[S, Interpreter[S, *]]

  def transpile[S[_]](ast: Ast[S]): Interpreter[S, Raw] = {
    import monocle.syntax.all.*

    implicit val re: ReportError[S, CompilerState[S]] =
      (st: CompilerState[S], token: Token[S], hint: String) =>
        st.focus(_.errors).modify(_.append(RulesViolated(token, hint)))

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

  def process[S[_]](ast: Ast[S], init: RawContext)(implicit
    aqum: Monoid[RawContext]
  ): ValidatedNec[SemanticError[S], RawContext] =
    astToState[S](ast)
      .run(CompilerState.init[S](init))
      .map {
        case (state, gen: RawContext) =>
          val ctx: RawContext = gen // TODO handle constants RawContext.fromRawContext(gen, init)
          NonEmptyChain
            .fromChain(state.errors)
            .fold[ValidatedNec[SemanticError[S], RawContext]](Valid(ctx))(Invalid(_))
        case (state, _: Raw.Empty) =>
          NonEmptyChain
            .fromChain(state.errors)
            .fold[ValidatedNec[SemanticError[S], RawContext]](Valid(init))(Invalid(_))
        case (state, part: (RawPart | RawPart.Parts)) =>
          val ctx = RawContext.blank.copy(parts = RawPart.contextPart(part))
          NonEmptyChain
            .fromChain(state.errors)
            .fold[ValidatedNec[SemanticError[S], RawContext]](Valid(ctx))(Invalid(_))
        case (state, m) =>
          logger.error("Got unexpected " + m)
          NonEmptyChain
            .fromChain(state.errors)
            .map(Invalid(_))
            .getOrElse(Validated.invalidNec[SemanticError[S], RawContext](WrongAST(ast)))
      }
      // TODO: return as Eval
      .value
}
