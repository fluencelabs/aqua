package aqua.semantics

import aqua.model.Model
import aqua.model.func.body.FuncOp
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
import cats.data._
import cats.free.Free
import cats.syntax.apply._
import cats.syntax.semigroup._
import monocle.Lens
import monocle.macros.GenLens

object Semantics {

  def folder[F[_], G[_]](implicit
    A: AbilitiesAlgebra[F, G],
    N: NamesAlgebra[F, G],
    T: TypesAlgebra[F, G]
  ): (Expr[F], Chain[Free[G, Model]]) => Eval[Free[G, Model]] = { case (expr, inners) =>
    Eval later ExprSem
      .getProg[F, G](expr)
      .apply(
        // TODO instead of foldRight, do slidingWindow for 2 elements, merge right associative ones
        // Then foldLeft just like now
        inners
          .foldRight[Free[G, List[Model]]](Free.pure[G, List[Model]](List.empty[Model])) {
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

  type Alg0[F[_], A] = EitherK[AbilityOp[F, *], NameOp[F, *], A]
  type Alg[F[_], A] = EitherK[TypeOp[F, *], Alg0[F, *], A]

  def transpile[F[_]](ast: Ast[F]): Free[Alg[F, *], Model] =
    ast.cata(folder[F, Alg[F, *]]).value

  def interpret[F[_]](free: Free[Alg[F, *], Model]): State[CompilerState[F], Model] = {
    import monocle.macros.syntax.all._

    implicit val re: ReportError[F, CompilerState[F]] =
      (st: CompilerState[F], token: Token[F], hint: String) =>
        st.focus(_.errors).modify(_.append(token -> hint))

    implicit val ns: Lens[CompilerState[F], NamesState[F]] = GenLens[CompilerState[F]](_.names)

    val names = new NamesInterpreter[F, CompilerState[F]]()

    implicit val as: Lens[CompilerState[F], AbilitiesState[F]] =
      GenLens[CompilerState[F]](_.abilities)

    val abilities = new AbilitiesInterpreter[F, CompilerState[F]]()

    implicit val ts: Lens[CompilerState[F], TypesState[F]] = GenLens[CompilerState[F]](_.types)

    val types = new TypesInterpreter[F, CompilerState[F]]()

    val interpreter0: FunctionK[Alg0[F, *], State[CompilerState[F], *]] = abilities or names
    val interpreter: FunctionK[Alg[F, *], State[CompilerState[F], *]] = types or interpreter0

    free.foldMap[State[CompilerState[F], *]](interpreter)
  }

  def astToState[F[_]](ast: Ast[F]): State[CompilerState[F], Model] =
    (transpile[F] _ andThen interpret[F])(ast)
}
