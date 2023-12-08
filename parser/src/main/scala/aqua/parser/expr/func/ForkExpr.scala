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

case class ForkExpr[F[_]](
  item: Name[F],
  iterable: ValueToken[F]
) extends Expr[F](ForkExpr, item) {

  override def mapK[K[_]: Comonad](fk: F ~> K): ForkExpr[K] =
    copy(item.mapK(fk), iterable.mapK(fk))
}

object ForkExpr extends Expr.AndIndented {
  override def validChildren: List[Expr.Lexem] = ArrowExpr.funcChildren

  override def p: P[ForkExpr[Span.S]] =
    ((`fork` *> ` ` *> Name.p <* ` <- `) ~ ValueToken.`value`).map { case (item, iterable) =>
      ForkExpr(item, iterable)
    }
}
