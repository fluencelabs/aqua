package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Comonad
import cats.parse.Parser
import aqua.parser.lexer.Token._

case class ElseOtherwiseExpr[F[_]](point: F[Unit]) extends Expr[F]

object ElseOtherwiseExpr extends Expr.AndIndented(Expr.defer(OnExpr), ParExpr, CoalgebraExpr, AbilityIdExpr, ForExpr) {

  override def p[F[_]: LiftParser: Comonad]: Parser[Expr[F]] =
    (`else` | `otherwise`).lift.map(ElseOtherwiseExpr(_))
}
