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

package aqua.backend.ts

import aqua.backend.{FuncTypes, ServiceTypes, Types}
import aqua.res.{FuncRes, ServiceRes}

case class TypeScriptTypes(client: String) extends Types {
  override def typed(field: String, t: String): String = s"$field: $t"
  override def generic(field: String, t: String): String = s"$field<$t>"
  override def bang(field: String): String = s"$field!"
  def funcType(f: FuncRes): FuncTypes = TSFuncTypes(f, client)
  def serviceType(s: ServiceRes): ServiceTypes = TSServiceTypes(s, client)
}
