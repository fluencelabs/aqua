package aqua.parser.head

import aqua.parser.lexer.Token.*
import aqua.parser.lexer.Token
import aqua.parser.lexer.{Ability, Literal, Name, Value}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import cats.Comonad
import cats.parse.Parser

case class ModuleExpr[F[_]](
  name: Ability[F],
  exportAll: Option[Token[F]],
  exportNames: List[Name[F]],
  exportCustom: List[Ability[F]]
) extends HeaderExpr[F]

object ModuleExpr extends HeaderExpr.Leaf {

  type NameOrAb[F[_]] = Either[Name[F], Ability[F]]

  def nameOrAb[F[_]: LiftParser: Comonad]: Parser[NameOrAb[F]] =
    Name.p[F].map(Left(_)) | Ability.ab[F].map(Right(_))

  def nameOrAbList[F[_]: LiftParser: Comonad]: Parser[List[NameOrAb[F]]] =
    comma[NameOrAb[F]](nameOrAb[F]).map(_.toList)

  def nameOrAbListOrAll[F[_]: LiftParser: Comonad]: Parser[Either[List[NameOrAb[F]], Token[F]]] =
    nameOrAbList[F].map(Left(_)) | `star`.lift.map(Token.lift(_)).map(Right(_))

  override def p[F[_]: LiftParser: Comonad]: Parser[ModuleExpr[F]] =
    (`module` *> ` ` *> Ability.ab[F] ~
      (` exports ` *> nameOrAbListOrAll[F]).?).map {
      case (name, None) =>
        ModuleExpr(name, None, Nil, Nil)
      case (name, Some(Left(exportMembers))) =>
        ModuleExpr(
          name,
          None,
          exportMembers.collect { case Left(x) => x },
          exportMembers.collect { case Right(x) => x }
        )
      case (name, Some(Right(point))) =>
        ModuleExpr(
          name,
          Some(point),
          Nil,
          Nil
        )
    }

}
