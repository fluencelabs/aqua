package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.ParExpr
import aqua.parser.lexer.Token
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import cats.parse.Parser
import cats.{~>, Comonad}
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class ParExpr[F[_]](point: Token[F]) extends Expr[F](ParExpr, point) {

  override def mapK[K[_]: Comonad](fk: F ~> K): ParExpr[K] =
    copy(point.mapK(fk))
}

object ParExpr extends Expr.Prefix() {

  override def continueWith: List[Expr.Lexem] =
    OnExpr :: ForExpr :: JoinExpr :: CallArrowExpr :: Nil

  override val p: Parser[Expr[Span.S]] =
    `par`.lift.map(Token.lift[Span.S, Unit](_)).map(ParExpr(_))

}
