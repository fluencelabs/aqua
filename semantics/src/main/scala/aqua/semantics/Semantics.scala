package aqua.semantics

import aqua.model.func.raw.FuncOp
import aqua.model.{AquaContext, EmptyModel, Model, ScriptModel}
import aqua.parser.lexer.Token
import aqua.parser.{Ast, Expr}
import aqua.semantics.rules.ReportError
import aqua.semantics.rules.abilities.{
  AbilitiesAlgebra,
  AbilitiesInterpreter,
  AbilitiesState,
  AbilityOp
}
import aqua.semantics.rules.names.{NameOp, NamesAlgebra, NamesInterpreter, NamesState}
import aqua.semantics.rules.types.{TypeOp, TypesAlgebra, TypesInterpreter, TypesState}
import cats.Eval
import cats.arrow.FunctionK
import cats.data.Validated.{Invalid, Valid}
import cats.data.*
import cats.free.Free
import cats.kernel.Monoid
import cats.syntax.apply.*
import cats.syntax.semigroup.*
import monocle.Lens
import monocle.macros.GenLens
import scribe.Logging
import cats.Monad
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._

object Semantics extends Logging {

  def folder[S[_], G[_]: Monad](implicit
    A: AbilitiesAlgebra[S, G],
    N: NamesAlgebra[S, G],
    T: TypesAlgebra[S, G]
  ): (Expr[S], Chain[G[Model]]) => Eval[G[Model]] = { case (expr, inners) =>
    Eval later ExprSem
      .getProg[S, G](expr)
      .apply(
        // TODO instead of foldRight, do slidingWindow for 2 elements, merge right associative ones
        // Then foldLeft just like now
        inners
          .foldRight[G[List[Model]]](List.empty[Model].pure[G]) {
            case (a, b) =>
              (a, b).mapN {
                case (prev: FuncOp, (next: FuncOp) :: tail) if next.isRightAssoc =>
                  (prev :+: next) :: tail
                case (prev, acc) => prev :: acc
              }
          }
          .map(_.reduceLeftOption(_ |+| _).getOrElse(Model.empty("AST is empty")))
      )
  }

  type Alg0[S[_], A] = EitherK[AbilityOp[S, *], NameOp[S, *], A]
  type Alg[S[_], A] = EitherK[TypeOp[S, *], Alg0[S, *], A]

  def transpile[S[_]](ast: Ast[S]): Free[Alg[S, *], Model] =
    ast.cata(folder[S, Alg[S, *]]).value

  def interpret[S[_]](free: Free[Alg[S, *], Model]): State[CompilerState[S], Model] = {
    import monocle.syntax.all._

    implicit val re: ReportError[S, CompilerState[S]] =
      (st: CompilerState[S], token: Token[S], hint: String) =>
        st.focus(_.errors).modify(_.append(RulesViolated(token, hint)))

    implicit val ns: Lens[CompilerState[S], NamesState[S]] = GenLens[CompilerState[S]](_.names)

    val names = new NamesInterpreter[S, CompilerState[S]]()

    implicit val as: Lens[CompilerState[S], AbilitiesState[S]] =
      GenLens[CompilerState[S]](_.abilities)

    val abilities = new AbilitiesInterpreter[S, CompilerState[S]]()

    implicit val ts: Lens[CompilerState[S], TypesState[S]] = GenLens[CompilerState[S]](_.types)

    val types = new TypesInterpreter[S, CompilerState[S]]()

    val interpreter = new TypesInterpreter[S, CompilerState[S]]
      with AbilitiesInterpreter[S, CompilerState[S]]

    val interpreter0: FunctionK[Alg0[S, *], State[CompilerState[S], *]] = abilities or names
    val interpreter: FunctionK[Alg[S, *], State[CompilerState[S], *]] = types or interpreter0

    free.foldMap[State[CompilerState[S], *]](interpreter)
  }

  private def astToState[S[_]](ast: Ast[S]): State[CompilerState[S], Model] =
    (transpile[S] _ andThen interpret[S])(ast)

  def process[S[_]](ast: Ast[S], init: AquaContext)(implicit
    aqum: Monoid[AquaContext]
  ): ValidatedNec[SemanticError[S], AquaContext] =
    astToState[S](ast)
      .run(CompilerState.init[S](init))
      .map {
        case (state, gen: ScriptModel) =>
          val ctx = AquaContext.fromScriptModel(gen, init)
          NonEmptyChain
            .fromChain(state.errors)
            .fold[ValidatedNec[SemanticError[S], AquaContext]](Valid(ctx))(Invalid(_))
        case (state, _: EmptyModel) =>
          NonEmptyChain
            .fromChain(state.errors)
            .fold[ValidatedNec[SemanticError[S], AquaContext]](Valid(init))(Invalid(_))
        case (state, m) =>
          NonEmptyChain
            .fromChain(state.errors)
            .map(Invalid(_))
            .getOrElse(Validated.invalidNec[SemanticError[S], AquaContext](WrongAST(ast)))
      }
      // TODO: return as Eval
      .value
}
