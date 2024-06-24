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

package aqua.res

import aqua.model.{LiteralModel, ServiceModel}
import aqua.types.{ArrowType, ScalarType}

// TODO: docs
case class ServiceRes(name: String, members: List[(String, ArrowType)], defaultId: Option[String])

object ServiceRes {

  def fromModel(sm: ServiceModel): ServiceRes =
    ServiceRes(
      name = sm.name,
      members = sm.`type`.arrows.toList,
      defaultId = sm.defaultId.collect {
        case LiteralModel(value, t) if ScalarType.string.acceptsValueOf(t) =>
          value
      }
    )
}
