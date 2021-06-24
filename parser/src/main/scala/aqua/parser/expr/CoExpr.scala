package aqua.parser.expr

import aqua.parser.Expr
import aqua.parser.lexer.Token
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Comonad
import cats.parse.Parser
import Token.`co`

case class CoExpr[F[_]](point: Token[F]) extends Expr[F](CoExpr, point)

object CoExpr extends Expr.Prefix {
  override def continueWith: List[Expr.Lexem] = ParExpr.continueWith

  override def p[F[_]: LiftParser: Comonad]: Parser[Expr[F]] =
    `co`.lift.map(Token.lift[F, Unit](_)).map(CoExpr(_))

}
