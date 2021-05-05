package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Comonad
import cats.parse.Parser

case class ParExpr[F[_]](point: F[Unit]) extends Expr[F]

object ParExpr extends Expr.AndThen {

  override def validChildren: List[Expr.Companion] =
    List(Expr.defer(OnExpr), CallArrowExpr, Expr.defer(ForExpr))

  override def p[F[_]: LiftParser: Comonad]: Parser[ParExpr[F]] =
    `par`.lift.map(ParExpr(_))
}
