package aqua.parser.head

import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Literal, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class ImportExpr[F[_]](filename: Literal[F]) extends FilenameExpr[F] {

  override def mapK[K[_]: Comonad](fk: F ~> K): ImportExpr[K] =
    copy(filename.mapK(fk))

  override def toString: String = s"import ${filename.value}"
}

object ImportExpr extends HeaderExpr.Leaf {

  override val p: Parser[HeaderExpr[Span.S]] =
    `import` *> ` ` *> Value.string.map(ImportExpr(_))
}
