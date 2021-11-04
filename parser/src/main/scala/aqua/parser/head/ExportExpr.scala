package aqua.parser.head

import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Literal, Token, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.data.NonEmptyList
import cats.parse.Parser
import cats.syntax.either.*
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class ExportExpr[F[_]](pubs: NonEmptyList[FromExpr.NameOrAbAs[F]]) extends HeaderExpr[F] {

  override def token: Token[F] =
    pubs.head.bimap(_._1, _._1).fold(identity, identity)

  override def mapK[K[_]: Comonad](fk: F ~> K): ExportExpr[K] =
    copy(FromExpr.mapK(pubs)(fk))
}

object ExportExpr extends HeaderExpr.Leaf {

  override val p: Parser[ExportExpr[Span.S]] =
    (`_export` *> ` `) *> comma(FromExpr.nameOrAbAs).map(ExportExpr(_))
}
