package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.*
import aqua.parser.lexer.Token.{`parsec`, *}
import aqua.parser.lexer.{Name, ValueToken}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import cats.parse.Parser as P
import cats.syntax.comonad.*
import cats.{~>, Comonad}
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class ParSecExpr[F[_]](
  item: Name[F],
  iterable: ValueToken[F],
  peerId: ValueToken[F],
  via: List[ValueToken[F]]
) extends Expr[F](ParSecExpr, item) {

  override def mapK[K[_]: Comonad](fk: F ~> K): ParSecExpr[K] =
    copy(item.mapK(fk), iterable.mapK(fk), peerId.mapK(fk), via.map(_.mapK(fk)))
}

object ParSecExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] = ArrowExpr.funcChildren

  private lazy val parsecPart = (`parsec` *> ` ` *> Name.p <* ` <- `) ~ ValueToken.`value`

  private lazy val onPart =
    `on` *> ` ` *> ValueToken.`value` ~ (` ` *> `via` *> ` ` *> ValueToken.`value`).rep0

  override def p: P[ParSecExpr[Span.S]] =
    ((parsecPart <* ` `) ~ onPart).map { case ((item, iterable), (peerId, via)) =>
      ParSecExpr(item, iterable, peerId, via)
    }
}
