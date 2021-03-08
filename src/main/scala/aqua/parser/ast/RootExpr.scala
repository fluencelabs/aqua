package aqua.parser.ast

case class RootExpr[F[_]]() extends Expr[F] {}

object RootExpr
