package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.{IfExpr, TryExpr}
import aqua.parser.lexer.Token
import aqua.parser.lexer.Token.*
import aqua.parser.lift.{LiftParser, Span}
import aqua.parser.lift.LiftParser.*
import cats.parse.Parser as P
import cats.{Comonad, ~>}
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class TryExpr[F[_]](point: Token[F]) extends Expr[F](TryExpr, point) {

  override def mapK[K[_]: Comonad](fk: F ~> K): TryExpr[K] =
    copy(point.mapK(fk))
}

object TryExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] =
    IfExpr.validChildren

  override val p: P[TryExpr[Span.S]] =
    `try`.lift.map(Token.lift[Span.S, Unit](_)).map(TryExpr(_))
}
