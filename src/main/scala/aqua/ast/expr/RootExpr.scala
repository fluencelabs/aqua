package aqua.ast.expr

import aqua.ast.{Expr, Gen, Prog}

case class RootExpr[F[_]]() extends Expr[F] {

  def program[Alg[_]]: Prog[Alg, Gen] =
    Prog.noop
}

object RootExpr
