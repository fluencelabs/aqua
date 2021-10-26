package aqua.parser.head

import aqua.parser.lexer.Token.*
import aqua.parser.lexer.Token
import aqua.parser.lexer.{Ability, Literal, Name, Value}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import cats.Comonad
import cats.parse.Parser
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class ModuleExpr[F[_]](
  name: Ability[F],
  declareAll: Option[Token[F]],
  declareNames: List[Name[F]],
  declareCustom: List[Ability[F]]
) extends HeaderExpr[F] {
  override def token: Token[F] = name

  override def mapK[K[_]: Comonad](fk: F ~> K): ModuleExpr[K] =
    copy(
      name.mapK(fk),
      declareAll.map(_.mapK(fk)),
      declareNames.map(_.mapK(fk)),
      declareCustom.map(_.mapK(fk))
    )
}

object ModuleExpr extends HeaderExpr.Leaf {

  type NameOrAb[F[_]] = Either[Name[F], Ability[F]]

  val nameOrAb: Parser[NameOrAb[Span.F]] =
    Name.p.map(Left(_)) | Ability.ab.map(Right(_))

  val nameOrAbList: Parser[List[NameOrAb[Span.F]]] =
    comma[NameOrAb[Span.F]](nameOrAb).map(_.toList)

  val nameOrAbListOrAll: Parser[Either[List[NameOrAb[Span.F]], Token[Span.F]]] =
    nameOrAbList.map(Left(_)) | `star`.lift.map(Token.lift(_)).map(Right(_))

  override val p: Parser[ModuleExpr[Span.F]] =
    (`module` *> ` ` *> Ability.dotted ~
      (` declares ` *> nameOrAbListOrAll).?).map {
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
