package aqua.parser.expr

import aqua.parser.Expr

case class RootExpr[F[_]]() extends Expr[F]
