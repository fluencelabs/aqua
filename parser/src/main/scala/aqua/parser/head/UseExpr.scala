package aqua.parser.head

import aqua.parser.lexer.QName
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{LiteralToken, ValueToken}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.Comonad
import cats.parse.Parser
import cats.~>

case class UseExpr[F[_]](
  filename: LiteralToken[F],
  asModule: Option[QName[F]]
) extends FilenameExpr[F] {

  override def mapK[K[_]: Comonad](fk: F ~> K): UseExpr[K] =
    copy(filename.mapK(fk), asModule.map(_.mapK(fk)))

  override def toString: String =
    s"use ${filename.value}${asModule.map(_.value).fold("")(" as " + _)}"
}

object UseExpr extends HeaderExpr.Companion {

  override val p: Parser[HeaderExpr[Span.S]] = (
    `use` *> ` ` *> ValueToken.string ~
      (` as `.backtrack *> QName.p).?
  ).map { case (filename, asModule) =>
    UseExpr(filename, asModule)
  }
}
