package aqua.parser.head

import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Ability, Literal, Name, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.data.NonEmptyList
import cats.parse.Parser
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class UseFromExpr[F[_]](
  imports: NonEmptyList[FromExpr.NameOrAbAs[F]],
  filename: Literal[F],
  asModule: Ability[F]
) extends FilenameExpr[F] with FromExpr[F] {

  override def mapK[K[_]: Comonad](fk: F ~> K): UseFromExpr[K] =
    copy(FromExpr.mapK(imports)(fk), filename.mapK(fk), asModule.mapK(fk))

  override def toString(): String =
    s"use ${FromExpr.show(imports)} from ${filename.value} as ${asModule.value}"
}

object UseFromExpr extends HeaderExpr.Leaf {

  override val p: Parser[UseFromExpr[Span.F]] =
    (`use` *> FromExpr.importFrom.surroundedBy(` `) ~ Value
      .string ~ (` as ` *> Ability.ab)).map { case ((imports, filename), asModule) =>
      UseFromExpr(imports, filename, asModule)
    }
}
