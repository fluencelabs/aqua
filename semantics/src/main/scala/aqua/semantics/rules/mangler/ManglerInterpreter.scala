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

package aqua.semantics.rules.mangler

import aqua.mangler.ManglerState

import cats.data.State
import monocle.Lens

class ManglerInterpreter[X](using
  lens: Lens[X, ManglerState]
) extends ManglerAlgebra[State[X, *]] {

  override def rename(name: String): State[X, String] =
    apply(_.forbidAndRename(name))

  private def apply[A](f: ManglerState => (ManglerState, A)): State[X, A] =
    State.apply(lens.modifyF(f andThen (_.swap)) andThen (_.swap))
}
