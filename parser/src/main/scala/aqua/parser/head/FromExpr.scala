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

import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Ability, Name}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.Comonad
import cats.data.NonEmptyList
import cats.parse.Parser as P
import cats.syntax.bifunctor.*
import cats.~>

trait FromExpr[F[_]] {
  def imports: NonEmptyList[FromExpr.NameOrAbAs[F]]
}

object FromExpr {

  def mapK[F[_], K[_]: Comonad](
    imports: NonEmptyList[FromExpr.NameOrAbAs[F]]
  )(fk: F ~> K): NonEmptyList[FromExpr.NameOrAbAs[K]] =
    imports.map {
      case Left((n, nOp)) => Left((n.mapK(fk), nOp.map(_.mapK(fk))))
      case Right(a, aOp) => Right((a.mapK(fk), aOp.map(_.mapK(fk))))
    }

  type NameOrAbAs[F[_]] = Either[Name.As[F], Ability.As[F]]

  val nameOrAbAs: P[NameOrAbAs[Span.S]] =
    Name.nameAs.map(Left(_)) | Ability.abAs.map(Right(_))

  val importFrom: P[NonEmptyList[NameOrAbAs[Span.S]]] =
    comma(nameOrAbAs) <* ` ` <* `from`

  def show[F[_]](ne: NonEmptyList[NameOrAbAs[F]]): String =
    ne.toList
      .map(
        _.bimap(
          _.bimap(_.value, _.map(_.value)),
          _.bimap(_.value, _.map(_.value))
        ).map { case (name, rename) =>
          s"$name${rename.fold("")(" as " + _)}"
        }
      )
      .mkString(", ")
}
