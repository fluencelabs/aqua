package aqua

import aqua.ast.expr.{
  AbilityIdExpr,
  AliasExpr,
  ArrowTypeExpr,
  CoalgebraExpr,
  DataStructExpr,
  FieldTypeExpr,
  FuncExpr,
  OnExpr,
  ParExpr,
  RootExpr,
  ServiceExpr
}
import aqua.ast.algebra.abilities.{AbilitiesAlgebra, AbilityOp}
import aqua.ast.algebra.names.{NameOp, NamesAlgebra}
import aqua.ast.algebra.scope.{PeerIdAlgebra, PeerIdOp}
import aqua.ast.algebra.types.{TypeOp, TypesAlgebra}
import aqua.ast.{Ast, Expr, Prog}
import cats.data.EitherK
import cats.{Comonad, Eval}
import cats.free.Free
import cats.syntax.flatMap._

object Compiler {

  private def exprToProg[F[_]: Comonad, G[_]](
    expr: Expr[F]
  )(implicit
    A: AbilitiesAlgebra[F, G],
    N: NamesAlgebra[F, G],
    P: PeerIdAlgebra[F, G],
    T: TypesAlgebra[F, G]
  ): Prog[G, Unit] =
    expr match {
      case expr: AbilityIdExpr[F] => expr.program[G]
      case expr: AliasExpr[F] => expr.program[G]
      case expr: ArrowTypeExpr[F] => expr.program[G]
      case expr: CoalgebraExpr[F] => expr.program[G]
      case expr: DataStructExpr[F] => expr.program[G]
      case expr: FieldTypeExpr[F] => expr.program[G]
      case expr: FuncExpr[F] => expr.program[G]
      case expr: OnExpr[F] => expr.program[G]
      case expr: ParExpr[F] => expr.program[G]
      case expr: ServiceExpr[F] => expr.program[G]
      case _: RootExpr[F] => Free.pure[G, Unit](())
    }

  def folder[F[_]: Comonad, G[_]](implicit
    A: AbilitiesAlgebra[F, G],
    N: NamesAlgebra[F, G],
    P: PeerIdAlgebra[F, G],
    T: TypesAlgebra[F, G]
  ): (Expr[F], List[Free[G, Unit]]) => Eval[Free[G, Unit]] = {
    case (expr, inners) =>
      Eval later exprToProg[F, G](expr).apply(inners.foldLeft(Free.pure[G, Unit](()))(_ >> _))
  }

  type Alg0[F[_], A] = EitherK[AbilityOp.Aux[F, *], NameOp.Aux[F, *], A]
  type Alg1[F[_], A] = EitherK[PeerIdOp[F, *], Alg0[F, *], A]
  type Alg[F[_], A] = EitherK[TypeOp[F, *], Alg1[F, *], A]

  def transpile[F[_]: Comonad](ast: Ast[F]): Free[Alg[F, *], Unit] =
    ast.cata(folder[F, Alg[F, *]]).value

}
