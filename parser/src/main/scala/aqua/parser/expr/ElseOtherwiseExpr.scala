package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Comonad
import cats.parse.Parser

case class ElseOtherwiseExpr[F[_]](point: F[Unit]) extends Expr[F](ElseOtherwiseExpr)

object ElseOtherwiseExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] = ForExpr.validChildren

  override def p[F[_]: LiftParser: Comonad]: Parser[ElseOtherwiseExpr[F]] =
    (`else` | `otherwise`).lift.map(ElseOtherwiseExpr(_))
}
