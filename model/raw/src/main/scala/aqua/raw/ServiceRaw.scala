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

package aqua.raw

import aqua.types.{ServiceType, Type}
import aqua.raw.value.ValueRaw

case class ServiceRaw(
  name: String,
  `type`: ServiceType,
  defaultId: Option[ValueRaw]
) extends RawPart {
  def rawPartType: ServiceType = `type`

  override def rename(s: String): RawPart = copy(name = s)

  def addAbilityName(s: String): RawPart = copy(`type` = Type.addAbilityNameService(s, `type`))
}
