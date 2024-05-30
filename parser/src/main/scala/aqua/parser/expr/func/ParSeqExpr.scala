package aqua.parser.expr.func

import aqua.parser.Expr
import aqua.parser.expr.*
import aqua.parser.lexer.Token.{`parseq`, *}
import aqua.parser.lexer.{Name, ValueToken}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.parse.Parser as P
import cats.syntax.comonad.*
import cats.syntax.either.*
import cats.{Comonad, ~>}

case class ParSeqExpr[F[_]](
  item: ForExpr.NameOrPair[F],
  iterable: ValueToken[F],
  peerId: ValueToken[F],
  via: List[ValueToken[F]]
) extends Expr[F](ParSeqExpr, iterable) {

  override def mapK[K[_]: Comonad](fk: F ~> K): ParSeqExpr[K] =
    copy(
      item.bimap(p => (p._1.mapK(fk), p._2.mapK(fk)), v => v.mapK(fk)),
      iterable.mapK(fk),
      peerId.mapK(fk),
      via.map(_.mapK(fk))
    )
}

object ParSeqExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] = ArrowExpr.funcChildren

  private lazy val parseqPart =
    (`parseq` *> ` ` *> ForExpr.nameOrPair <* ` <- `) ~ ValueToken.`value`

  private lazy val onPart =
    `on` *> ` ` *> ValueToken.`value` ~ (` ` *> `via` *> ` ` *> ValueToken.`value`).rep0

  override def p: P[ParSeqExpr[Span.S]] =
    ((parseqPart <* ` `) ~ onPart).map { case ((item, iterable), (peerId, via)) =>
      ParSeqExpr(item, iterable, peerId, via)
    }
}
