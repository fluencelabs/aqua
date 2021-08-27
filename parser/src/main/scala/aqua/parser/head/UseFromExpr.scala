package aqua.parser.head

import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Ability, Literal, Name, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.data.NonEmptyList
import cats.parse.Parser
import cats.~>

case class UseFromExpr[F[_]](
  imports: NonEmptyList[FromExpr.NameOrAbAs[F]],
  filename: Literal[F],
  asModule: Ability[F]
) extends FilenameExpr[F] with FromExpr[F] {

  override def mapK[K[_]: Comonad](fk: F ~> K): UseFromExpr[K] =
    copy(FromExpr.mapK(imports)(fk), filename.mapK(fk), asModule.mapK(fk))
}

object UseFromExpr extends HeaderExpr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[HeaderExpr[F]] =
    (`use` *> FromExpr.importFrom[F].surroundedBy(` `) ~ Value
      .string[F] ~ (` as ` *> Ability.ab[F])).map { case ((imports, filename), asModule) =>
      UseFromExpr(imports, filename, asModule)
    }
}
