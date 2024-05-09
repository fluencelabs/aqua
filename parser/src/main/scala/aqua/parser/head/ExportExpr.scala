package aqua.parser.head

import aqua.parser.lexer.QName
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{LiteralToken, Token, ValueToken}
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.Comonad
import cats.data.NonEmptyList
import cats.parse.Parser
import cats.syntax.comonad.*
import cats.syntax.functor.*
import cats.~>

case class ExportExpr[F[_]](
  token: Token[F],
  pubs: NonEmptyList[QName.As[F]]
) extends HeaderExpr[F] {

  override def mapK[K[_]: Comonad](fk: F ~> K): ExportExpr[K] =
    copy(token = token.mapK(fk), pubs = pubs.map(_.mapK(fk)))
}

object ExportExpr extends HeaderExpr.Companion {

  override val p: Parser[ExportExpr[Span.S]] =
    ((`export` *> ` `) *> comma(QName.as)).lift.map(point =>
      ExportExpr(
        Token.lift(point.void),
        point.extract
      )
    )
}
