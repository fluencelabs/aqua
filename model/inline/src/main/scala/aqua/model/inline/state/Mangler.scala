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

package aqua.model.inline.state

import cats.data.State
import aqua.mangler.ManglerState

trait Mangler[S] {
  self =>
  def findAndForbidName(introduce: String): State[S, String] =
    findAndForbidNames(Set(introduce)).map(_.getOrElse(introduce, introduce))

  def findAndForbidNames(introduce: Set[String]): State[S, Map[String, String]]

  def forbid(names: Set[String]): State[S, Unit]

  def transformS[R](f: R => S, g: (R, S) => R): Mangler[R] =
    new Mangler[R] {

      def forbid(names: Set[String]): State[R, Unit] =
        self.forbid(names).transformS(f, g)

      def findAndForbidNames(introduce: Set[String]): State[R, Map[String, String]] =
        self.findAndForbidNames(introduce).transformS(f, g)
    }
}

object Mangler {
  def apply[S](using mangler: Mangler[S]): Mangler[S] = mangler

  given Mangler[ManglerState] with {
    def findAndForbidNames(introduce: Set[String]): State[ManglerState, Map[String, String]] =
      State.apply(_.findNewNames(introduce))

    def forbid(names: Set[String]): State[ManglerState, Unit] =
      State.modify(st => st.forbid(names))
  }
}
