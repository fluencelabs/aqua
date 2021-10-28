package aqua.semantics

import aqua.model.func.raw.FuncOp
import aqua.model.{AquaContext, EmptyModel, Model, ScriptModel}
import aqua.parser.lexer.Token
import aqua.parser.{Ast, Expr}
import aqua.semantics.rules.abilities.{
  AbilitiesAlgebra,
  AbilitiesInterpreter,
  AbilitiesState
}
import aqua.semantics.rules.names.{NamesAlgebra, NamesInterpreter, NamesState}
import aqua.semantics.rules.types.{TypesAlgebra, TypesInterpreter, TypesState}
import aqua.semantics.rules.{ReportError, ValuesAlgebra}
import cats.{Eval, Monad}
import cats.arrow.FunctionK
import cats.data.*
import cats.data.Validated.{Invalid, Valid}
import cats.free.Free
import cats.kernel.Monoid
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.semigroup.*
import monocle.Lens
import monocle.macros.GenLens
import scribe.Logging

object Semantics extends Logging {

  def folder[S[_], G[_]: Monad](implicit
    A: AbilitiesAlgebra[S, G],
    N: NamesAlgebra[S, G],
    T: TypesAlgebra[S, G],
    V: ValuesAlgebra[S, G]
  ): (Expr[S], Chain[G[Model]]) => Eval[G[Model]] = { case (expr, inners) =>
    Eval later ExprSem
      .getProg[S, G](expr)
      .apply(
        // TODO instead of foldRight, do slidingWindow for 2 elements, merge right associative ones
        // Then foldLeft just like now
        inners
          .foldRight[G[List[Model]]](List.empty[Model].pure[G]) { case (a, b) =>
            (a, b).mapN {
              case (prev: FuncOp, (next: FuncOp) :: tail) if next.isRightAssoc =>
                (prev :+: next) :: tail
              case (prev, acc) => prev :: acc
            }
          }
          .map(_.reduceLeftOption(_ |+| _).getOrElse(Model.empty("AST is empty")))
      )
  }

  type Interpreter[S[_], A] = State[CompilerState[S], A]

  // todo: is it needed?
  type Alg[S[_]] = TypesAlgebra[S, Interpreter[S, *]]
    with NamesAlgebra[S, Interpreter[S, *]] with ValuesAlgebra[S, Interpreter[S, *]]
    with AbilitiesAlgebra[S, Interpreter[S, *]]

  def transpile[S[_]](ast: Ast[S]): Interpreter[S, Model] = {
    import monocle.syntax.all.*

    implicit val re: ReportError[S, CompilerState[S]] =
      (st: CompilerState[S], token: Token[S], hint: String) =>
        st.focus(_.errors).modify(_.append(RulesViolated(token, hint)))

    implicit val ns: Lens[CompilerState[S], NamesState[S]] = GenLens[CompilerState[S]](_.names)

    implicit val as: Lens[CompilerState[S], AbilitiesState[S]] =
      GenLens[CompilerState[S]](_.abilities)

    implicit val ts: Lens[CompilerState[S], TypesState[S]] = GenLens[CompilerState[S]](_.types)

    implicit val typesInterpreter: TypesInterpreter[S, CompilerState[S]] = new TypesInterpreter[S, CompilerState[S]]
    implicit val abilitiesInterpreter: AbilitiesInterpreter[S, CompilerState[S]] = new AbilitiesInterpreter[S, CompilerState[S]]
    implicit val namesInterpreter: NamesInterpreter[S, CompilerState[S]] = new NamesInterpreter[S, CompilerState[S]]
    implicit val valuesInterpreter: ValuesAlgebra[S, Interpreter[S, *]] = ValuesAlgebra.deriveValuesAlgebra[S, Interpreter[S, *]]

    ast.cata(folder[S, Interpreter[S, *]]).value
  }

  private def astToState[S[_]](ast: Ast[S]): Interpreter[S, Model] =
    transpile[S](ast)

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
