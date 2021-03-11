package aqua.ast.expr

import aqua.ast.Expr

case class RootExpr[F[_]]() extends Expr[F] {}

object RootExpr
