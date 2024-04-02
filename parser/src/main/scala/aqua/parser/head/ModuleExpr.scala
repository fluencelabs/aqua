package aqua.parser.head

import aqua.parser.lexer.Token
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Ability, LiteralToken, Name, ValueToken}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

import cats.Comonad
import cats.parse.Parser
import cats.syntax.applicative.*
import cats.syntax.comonad.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.~>

case class ModuleExpr[F[_]](
  word: ModuleExpr.Word[F],
  name: Ability[F],
  declareAll: Option[Token[F]],
  declareNames: List[Name[F]],
  declareCustom: List[Ability[F]]
) extends HeaderExpr[F] {
  override def token: Token[F] = name

  override def mapK[K[_]: Comonad](fk: F ~> K): ModuleExpr[K] =
    copy(
      word = word.mapK(fk),
      name = name.mapK(fk),
      declareAll = declareAll.map(_.mapK(fk)),
      declareNames = declareNames.map(_.mapK(fk)),
      declareCustom = declareCustom.map(_.mapK(fk))
    )
}

object ModuleExpr extends HeaderExpr.Companion {

  final case class Word[F[_]: Comonad](
    token: F[Word.Kind]
  ) extends Token[F] {
    override def mapK[K[_]: Comonad](fk: F ~> K): Word[K] = copy(fk(token))

    override def as[T](v: T): F[T] = token.as(v)

    def value: Word.Kind = token.extract
  }

  object Word {

    enum Kind {
      case Module, Aqua

      def fold[A](
        module: => A,
        aqua: => A
      ): A = this match {
        case Kind.Module => module
        case Kind.Aqua => aqua
      }
    }
  }

  type NameOrAb[F[_]] = Either[Name[F], Ability[F]]

  private val nameOrAb: Parser[NameOrAb[Span.S]] =
    Name.p.map(Left(_)) | Ability.ab.map(Right(_))

  private val nameOrAbList: Parser[List[NameOrAb[Span.S]]] =
    comma[NameOrAb[Span.S]](nameOrAb).map(_.toList)

  private val nameOrAbListOrAll: Parser[Either[List[NameOrAb[Span.S]], Token[Span.S]]] =
    nameOrAbList.map(Left(_)) | `star`.lift.map(Token.lift(_)).map(Right(_))

  private val moduleWord: Parser[Word[Span.S]] =
    (`module`.as(Word.Kind.Module).lift.backtrack |
      `aqua-word`.as(Word.Kind.Aqua).lift).map(Word(_))

  override val p: Parser[ModuleExpr[Span.S]] =
    (
      (` *`.with1 *> moduleWord) ~
        (` ` *> Ability.dotted) ~
        ` *`.flatMap(spaces =>
          // flatMap is costly, but there is no other way
          // to allow either a bunch of spaces or a declares part
          if (spaces.nonEmpty)
            (`declares` *> ` ` *> nameOrAbListOrAll).?
          else none.pure
        )
    ).map {
      case ((word, name), None) =>
        ModuleExpr(word, name, None, Nil, Nil)
      case ((word, name), Some(Left(exportMembers))) =>
        ModuleExpr(
          word,
          name,
          None,
          exportMembers.collect { case Left(x) => x },
          exportMembers.collect { case Right(x) => x }
        )
      case ((word, name), Some(Right(point))) =>
        ModuleExpr(
          word,
          name,
          Some(point),
          Nil,
          Nil
        )
    }

}
