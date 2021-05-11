package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.Expr.RootCompanion
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Comonad
import cats.parse.Parser

case class ElseOtherwiseExpr[F[_]](point: F[Unit]) extends Expr[F] {
  override def root: Boolean = true
}

object ElseOtherwiseExpr extends Expr.AndIndented with RootCompanion {

  override def validChildren: List[Expr.Companion] =
    List(
      Expr.defer(OnExpr),
      CallArrowExpr,
      AbilityIdExpr,
      Expr.defer(ForExpr),
      Expr.defer(IfExpr),
      Expr.defer(ElseOtherwiseExpr)
    )

  override def p[F[_]: LiftParser: Comonad]: Parser[ElseOtherwiseExpr[F]] =
    (`else` | `otherwise`).lift.map(ElseOtherwiseExpr(_))
}
