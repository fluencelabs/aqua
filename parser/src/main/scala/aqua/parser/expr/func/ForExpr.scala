package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.*
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Name, ValueToken}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

import cats.parse.Parser as P
import cats.syntax.comonad.*
import cats.{Comonad, ~>}

case class ForExpr[F[_]](
  item: Name[F],
  iterable: ValueToken[F],
  mode: Option[ForExpr.Mode]
) extends Expr[F](ForExpr, item) {

  override def mapK[K[_]: Comonad](fk: F ~> K): ForExpr[K] =
    copy(item.mapK(fk), iterable.mapK(fk))
}

object ForExpr extends Expr.AndIndented {
  enum Mode { case ParMode, TryMode, RecMode }

  override def validChildren: List[Expr.Lexem] = ArrowExpr.funcChildren

  private lazy val modeP: P[Mode] =
    (` ` *> (
      `par`.as(Mode.ParMode) |
        `try`.as(Mode.TryMode) |
        `rec`.as(Mode.RecMode)
    ).lift).map(_.extract)

  override def p: P[ForExpr[Span.S]] =
    ((`for` *> ` ` *> Name.p <* ` <- `) ~ ValueToken.`value` ~ modeP.?).map {
      case ((item, iterable), mode) =>
        ForExpr(item, iterable, mode)
    }
}
