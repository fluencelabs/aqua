package aqua.parser.head

import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Literal, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.data.NonEmptyList
import cats.parse.Parser

case class ImportFromExpr[F[_]](
  imports: NonEmptyList[FromExpr.NameOrAbAs[F]],
  filename: Literal[F]
) extends FilenameExpr[F] with FromExpr[F]

object ImportFromExpr extends HeaderExpr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[HeaderExpr[F]] =
    (`import` *> FromExpr.importFrom[F].surroundedBy(` `) ~ Value.string[F]).map {
      case (imports, filename) => ImportFromExpr(imports, filename)
    }
}
