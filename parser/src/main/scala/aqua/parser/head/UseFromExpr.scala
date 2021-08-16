package aqua.parser.head

import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Ability, Literal, Name, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.data.NonEmptyList
import cats.parse.Parser

case class UseFromExpr[F[_]](
  imports: NonEmptyList[UseFromExpr.NameOrAbAs[F]],
  filename: Literal[F],
  asModule: Ability[F]
) extends FilenameExpr[F]

object UseFromExpr extends HeaderExpr.Leaf {
  type NameAs[F[_]] = (Name[F], Option[Name[F]])

  private def nameAs[F[_]: LiftParser: Comonad]: Parser[NameAs[F]] =
    Name.p[F] ~ (` as ` *> Name.p[F]).?

  type AbAs[F[_]] = (Ability[F], Option[Ability[F]])

  private def abAs[F[_]: LiftParser: Comonad]: Parser[AbAs[F]] =
    Ability.ab[F] ~ (` as ` *> Ability.ab[F]).?

  type NameOrAbAs[F[_]] = Either[NameAs[F], AbAs[F]]

  private def nameOrAbAs[F[_]: LiftParser: Comonad]: Parser[NameOrAbAs[F]] =
    nameAs[F].map(Left(_)) | abAs[F].map(Right(_))

  override def p[F[_]: LiftParser: Comonad]: Parser[HeaderExpr[F]] =
    (`use` *> ` ` *> (comma[NameOrAbAs[F]](nameOrAbAs[F]) <* ` from `) ~ Value
      .string[F] ~ (` as ` *> Ability.ab[F])).map { case ((imports, filename), asModule) =>
      UseFromExpr(imports, filename, asModule)

    }
}
