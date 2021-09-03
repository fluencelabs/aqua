package aqua.parser.head

import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Literal, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.data.NonEmptyList
import cats.parse.Parser
import cats.~>

case class ImportFromExpr[F[_]](
  imports: NonEmptyList[FromExpr.NameOrAbAs[F]],
  filename: Literal[F]
) extends FilenameExpr[F] with FromExpr[F] {

  override def mapK[K[_]: Comonad](fk: F ~> K): ImportFromExpr[K] =
    copy(FromExpr.mapK(imports)(fk), filename.mapK(fk))

  override def toString: String = s"import ${FromExpr.show(imports)} from ${filename.value}"
}

object ImportFromExpr extends HeaderExpr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[HeaderExpr[F]] =
    (`import` *> FromExpr.importFrom[F].surroundedBy(` `) ~ Value.string[F]).map {
      case (imports, filename) => ImportFromExpr(imports, filename)
    }
}
