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

package aqua.parser.lexer

import aqua.helpers.data.PName
import aqua.helpers.data.SName
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.Comonad
import cats.parse.{Parser => P}
import cats.syntax.comonad.*
import cats.syntax.functor.*
import cats.~>

case class Name[F[_]: Comonad](name: F[String]) extends Token[F] {
  override def as[T](v: T): F[T] = name.as(v)

  def asTypeToken: NamedTypeToken[F] = NamedTypeToken(name)

  override def mapK[K[_]: Comonad](fk: F ~> K): Name[K] = copy(fk(name))

  def rename(newName: String): Name[F] = copy(name.as(newName))

  def value: String = name.extract

  /*
    WARNING: This method is unsafe. `Name[S]` could be a path name
   */
  def simpleName: SName = SName.nameUnsafe(value)

  def pathName: PName = PName.stringUnsafe(value)

  override def toString() = value
}

object Name {

  type As[F[_]] = (Name[F], Option[Name[F]])

  val p: P[Name[Span.S]] =
    `name`.lift.map(Name(_))

  val variable: P[Name[Span.S]] =
    (name | Class).lift.map(Name(_))

  val upper: P[Name[Span.S]] =
    NAME.lift.map(Name(_))

  val nameAs: P[As[Span.S]] =
    asOpt(p)
}
