package aqua.parser.head

import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{LiteralToken, ValueToken}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.Comonad
import cats.data.NonEmptyList
import cats.parse.Parser
import cats.~>

case class ImportFromExpr[F[_]](
  imports: FromExpr.Imports[F],
  filename: LiteralToken[F]
) extends FilenameExpr[F] with FromExpr[F] {

  override def mapK[K[_]: Comonad](fk: F ~> K): ImportFromExpr[K] =
    copy(FromExpr.mapK(imports)(fk), filename.mapK(fk))

  override def toString: String = s"import ${FromExpr.show(imports)} from ${filename.value}"
}

object ImportFromExpr extends HeaderExpr.Companion {

  override val p: Parser[HeaderExpr[Span.S]] = (
    `import` ~ ` ` *> FromExpr.importsP ~
      (` from ` *> ValueToken.string)
  ).map { case (imports, filename) =>
    ImportFromExpr(imports, filename)
  }
}
