package aqua

import aqua.parser.ast.{Expr, Prog, RootExpr}
import cats.free.Free
import cats.syntax.flatMap._

object Compiler {

  def exprToProg[Alg[_], F[_]](expr: Expr[F]): Prog[Alg, Unit] =
    expr match {
      case _: RootExpr[F] => Free.pure[Alg, Unit](())
    }

  def folder[Alg[_], F[_]]: (Expr[F], List[Free[Alg, Unit]]) => Free[Alg, Unit] = {
    case (expr, inners) =>
      exprToProg[Alg, F](expr)(inners.foldLeft(Free.pure[Alg, Unit](()))(_ >> _))
  }

}
