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

package aqua.model.inline.raw

import aqua.model.ValueModel
import aqua.model.inline.Inline
import aqua.model.inline.state.*
import aqua.raw.value.ValueRaw

import cats.data.State

trait RawInliner[T <: ValueRaw] {

  def apply[S: Mangler: Exports: Arrows: Config](
    raw: T,
    propertiesAllowed: Boolean = true
  ): State[S, (ValueModel, Inline)]

}
