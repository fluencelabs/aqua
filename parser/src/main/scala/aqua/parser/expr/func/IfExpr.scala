package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.func.{ForExpr, IfExpr}
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{LiteralToken, ValueToken}
import aqua.parser.lift.LiftParser
import aqua.types.LiteralType
import cats.parse.Parser as P
import cats.{~>, Comonad}
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class IfExpr[F[_]](value: ValueToken[F]) extends Expr[F](IfExpr, value) {

  override def mapK[K[_]: Comonad](fk: F ~> K): IfExpr[K] =
    copy(value.mapK(fk))
}

object IfExpr extends Expr.AndIndented {

  // list of expressions that can be used inside this block
  override def validChildren: List[Expr.Lexem] = ForExpr.validChildren

  override val p: P[IfExpr[Span.S]] =
    (`if` *> ` ` *> ValueToken.`value`).map(IfExpr(_))
}
