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

import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.Comonad
import cats.parse.Parser as P
import cats.syntax.comonad.*
import cats.syntax.functor.*
import cats.~>

case class Ability[F[_]: Comonad](name: F[String]) extends Token[F] {
  override def as[T](v: T): F[T] = name.as(v)

  def mapK[K[_]: Comonad](fk: F ~> K): Ability[K] = copy(fk(name))

  def value: String = name.extract
}

object Ability {
  type As[F[_]] = (Ability[F], Option[Ability[F]])

  val ab: P[Ability[Span.S]] =
    `Class`.lift.map(Ability(_))

  val dotted: P[Ability[Span.S]] =
    P.repSep(`Class`, `.`).map(_.toList.mkString(".")).lift.map(Ability(_))

  val abAs: P[As[Span.S]] =
    asOpt(ab)
}
