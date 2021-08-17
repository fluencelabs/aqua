package aqua.parser.head

import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Literal, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.data.NonEmptyList
import cats.parse.Parser

case class ExportExpr[F[_]](pubs: NonEmptyList[FromExpr.NameOrAbAs[F]]) extends HeaderExpr[F]

object ExportExpr extends HeaderExpr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[ExportExpr[F]] =
    (`_export` *> ` `) *> comma(FromExpr.nameOrAbAs[F]).map(ExportExpr(_))
}
