package aqua

import aqua.interim.abilities.{AbilitiesAlgebra, AbilityOp}
import aqua.interim.names.{NameOp, NamesAlgebra}
import aqua.interim.scope.{PeerIdAlgebra, PeerIdOp}
import aqua.interim.types.{TypeOp, TypesAlgebra}
import aqua.parser.ast.{
  AbilityIdExpr,
  AliasExpr,
  ArrowTypeExpr,
  Ast,
  CoalgebraExpr,
  DataStructExpr,
  Expr,
  FieldTypeExpr,
  FuncExpr,
  OnExpr,
  ParExpr,
  Prog,
  RootExpr,
  ServiceExpr
}
import cats.data.EitherK
import cats.{Comonad, Eval, InjectK}
import cats.free.Free
import cats.syntax.flatMap._

object Compiler {

  private def exprToProg[Alg[_]: AbilitiesAlgebra: TypesAlgebra: PeerIdAlgebra: NamesAlgebra, F[_]: Comonad](
    expr: Expr[F]
  ): Prog[Alg, Unit] =
    expr match {
      case expr: AbilityIdExpr[F] => expr.program[Alg]
      case expr: AliasExpr[F] => expr.program[Alg]
      case expr: ArrowTypeExpr[F] => expr.program[Alg]
      case expr: CoalgebraExpr[F] => expr.program[Alg]
      case expr: DataStructExpr[F] => expr.program[Alg]
      case expr: FieldTypeExpr[F] => expr.program[Alg]
      case expr: FuncExpr[F] => expr.program[Alg]
      case expr: OnExpr[F] => expr.program[Alg]
      case expr: ParExpr[F] => expr.program[Alg]
      case expr: ServiceExpr[F] => expr.program[Alg]
      case _: RootExpr[F] => Free.pure[Alg, Unit](())
    }

  def folder[Alg[_]: AbilitiesAlgebra: TypesAlgebra: PeerIdAlgebra: NamesAlgebra, F[_]: Comonad]
    : (Expr[F], List[Free[Alg, Unit]]) => Eval[Free[Alg, Unit]] = {
    case (expr, inners) =>
      Eval later exprToProg[Alg, F](expr).apply(inners.foldLeft(Free.pure[Alg, Unit](()))(_ >> _))
  }

  type Alg0[A] = EitherK[AbilityOp, NameOp, A]
  type Alg1[A] = EitherK[PeerIdOp, Alg0, A]
  type Alg[A] = EitherK[TypeOp, Alg1, A]

  def transpile[F[_]: Comonad](ast: Ast[F]): Free[Alg, Unit] =
    ast.cata(folder[Alg, F]).value

}
