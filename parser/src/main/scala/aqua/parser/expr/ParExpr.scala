package aqua.parser.expr

import aqua.parser.Expr
import cats.Comonad
import cats.parse.Parser

import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._

case class ParExpr[F[_]](point: F[Unit]) extends Expr[F](ParExpr)

object ParExpr extends Expr.Prefix {
  override def continueWith: List[Expr.Lexem] = CallArrowExpr :: OnExpr :: ForExpr :: Nil

  override def p[F[_]: LiftParser: Comonad]: Parser[Expr[F]] = `par`.lift.map(ParExpr(_))

}
