package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.*
import aqua.parser.lexer.Token.{`parseq`, *}
import aqua.parser.lexer.{Name, ValueToken}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import cats.parse.Parser as P
import cats.syntax.comonad.*
import cats.{~>, Comonad}
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class ParSeqExpr[F[_]](
  item: Name[F],
  iterable: ValueToken[F],
  peerId: ValueToken[F],
  via: List[ValueToken[F]]
) extends Expr[F](ParSeqExpr, item) {

  override def mapK[K[_]: Comonad](fk: F ~> K): ParSeqExpr[K] =
    copy(item.mapK(fk), iterable.mapK(fk), peerId.mapK(fk), via.map(_.mapK(fk)))
}

object ParSeqExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] = ArrowExpr.funcChildren

  private lazy val parseqPart = (`parseq` *> ` ` *> Name.p <* ` <- `) ~ ValueToken.`value`

  private lazy val onPart =
    `on` *> ` ` *> ValueToken.`value` ~ (` ` *> `via` *> ` ` *> ValueToken.`value`).rep0

  override def p: P[ParSeqExpr[Span.S]] =
    ((parseqPart <* ` `) ~ onPart).map { case ((item, iterable), (peerId, via)) =>
      ParSeqExpr(item, iterable, peerId, via)
    }
}
