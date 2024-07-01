/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.parser.head

import aqua.parser.lexer.QName
import aqua.parser.lexer.Token
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Ability, LiteralToken, Name, ValueToken}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.Comonad
import cats.data.NonEmptyList
import cats.parse.Parser
import cats.syntax.applicative.*
import cats.syntax.comonad.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.~>

case class ModuleExpr[F[_]](
  word: ModuleExpr.Word[F],
  name: QName[F],
  declares: Option[ModuleExpr.Declares[F]]
) extends HeaderExpr[F] {
  override def token: Token[F] = name

  override def mapK[K[_]: Comonad](fk: F ~> K): ModuleExpr[K] =
    copy(
      word = word.mapK(fk),
      name = name.mapK(fk),
      declares = declares.map(_.mapK(fk))
    )
}

object ModuleExpr extends HeaderExpr.Companion {

  enum Declares[F[_]] {
    case All(point: Token[F])
    case Names(names: NonEmptyList[QName[F]])

    def mapK[K[_]: Comonad](fk: F ~> K): Declares[K] = this match {
      case All(point) => All(point.mapK(fk))
      case Names(names) => Names(names.map(_.mapK(fk)))
    }
  }

  object Declares {

    val p: Parser[Declares[Span.S]] =
      (`declares` ~ ` *`) *> (
        comma(QName.p).map(Names(_)) |
          `star`.lift.map(Token.lift).map(All(_))
      )
  }

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

    val p = (`module`.as(Word.Kind.Module).lift.backtrack |
      `aqua-word`.as(Word.Kind.Aqua).lift).map(Word(_))
  }

  override val p: Parser[ModuleExpr[Span.S]] =
    (
      (` *`.with1 *> Word.p) ~
        (` *` *> QName.p) ~
        (` *` *> Declares.p <* ` *`).backtrack
          .map(_.some)
          // Allow trailing spaces without `declares`
          .orElse(` *`.as(none))
    ).map { case ((word, name), declares) =>
      ModuleExpr(word, name, declares)
    }

}
