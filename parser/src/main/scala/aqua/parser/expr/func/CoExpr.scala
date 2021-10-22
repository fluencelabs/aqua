package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.{CoExpr, ParExpr}
import aqua.parser.lexer.Token
import aqua.parser.lexer.Token.`co`
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import cats.parse.Parser
import cats.{~>, Comonad}
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class CoExpr[F[_]](point: Token[F]) extends Expr[F](CoExpr, point) {
  def mapK[K[_]: Comonad](fk: F ~> K): CoExpr[K] = copy(point.mapK(fk))
}

object CoExpr extends Expr.Prefix() {
  override def continueWith: List[Expr.Lexem] = ParExpr.continueWith

  override val p: Parser[Expr[Span.F]] =
    `co`.lift.map(Token.lift[Span.F, Unit](_)).map(CoExpr(_))

}
