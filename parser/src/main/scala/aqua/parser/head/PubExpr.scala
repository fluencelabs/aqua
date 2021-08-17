package aqua.parser.head

import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Literal, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.data.NonEmptyList
import cats.parse.Parser

case class PubExpr[F[_]](pubs: NonEmptyList[FromExpr.NameOrAbAs[F]]) extends HeaderExpr[F]

object PubExpr extends HeaderExpr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[PubExpr[F]] =
    (`pub` *> ` `) *> comma(FromExpr.nameOrAbAs[F]).map(PubExpr(_))
}
