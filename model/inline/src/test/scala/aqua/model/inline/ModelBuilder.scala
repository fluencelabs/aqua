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

package aqua.model.inline

import aqua.model.{CallModel, CallServiceModel, LiteralModel, ValueModel, VarModel}

object ModelBuilder {

  def add(l: ValueModel, r: ValueModel)(o: VarModel): CallServiceModel =
    CallServiceModel(
      serviceId = "math",
      funcName = "add",
      args = List(l, r),
      result = o
    )
}
