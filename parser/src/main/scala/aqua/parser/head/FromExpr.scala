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
  def imports: FromExpr.Imports[F]
}

object FromExpr {

  type Imports[F[_]] = NonEmptyList[QName.As[F]]

  def mapK[F[_], K[_]: Comonad](imports: Imports[F])(fk: F ~> K): Imports[K] =
    imports.map(_.mapK(fk))

  val importsP: P[Imports[Span.S]] = comma(QName.as)

  def show[F[_]](imports: Imports[F]): String =
    imports.toList.map { case QName.As(name, rename) =>
      s"${name.value}${rename.fold("")(" as " + _.value)}"
    }.mkString(", ")
}
