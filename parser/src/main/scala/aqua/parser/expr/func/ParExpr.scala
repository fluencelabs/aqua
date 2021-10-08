package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.ParExpr
import aqua.parser.lexer.Token
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import cats.parse.Parser
import cats.{~>, Comonad}

case class ParExpr[F[_]](point: Token[F]) extends Expr[F](ParExpr, point) {

  override def mapK[K[_]: Comonad](fk: F ~> K): ParExpr[K] =
    copy(point.mapK(fk))
}

object ParExpr extends Expr.Prefix() {
  override def continueWith: List[Expr.Lexem] = CallArrowExpr :: OnExpr :: ForExpr :: Nil

  override def p[F[_]: LiftParser: Comonad]: Parser[Expr[F]] =
    `par`.lift.map(Token.lift[F, Unit](_)).map(ParExpr(_))

}
