package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.{ElseOtherwiseExpr, ForExpr}
import aqua.parser.lexer.Token
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import cats.parse.Parser
import cats.{Comonad, ~>}
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class ElseOtherwiseExpr[F[_]](point: Token[F]) extends Expr[F](ElseOtherwiseExpr, point) {
  override def mapK[K[_]: Comonad](fk: F ~> K): ElseOtherwiseExpr[K] = copy(point.mapK(fk))
}

object ElseOtherwiseExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] = ForExpr.validChildren

  override val p: Parser[ElseOtherwiseExpr[Span.F]] =
    (`else` | `otherwise`).lift.map(Token.lift[Span.F, Unit](_)).map(ElseOtherwiseExpr(_))
}
