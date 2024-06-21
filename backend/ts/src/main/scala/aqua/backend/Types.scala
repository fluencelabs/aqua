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

package aqua.backend

import aqua.backend.ts.{TSFuncTypes, TSServiceTypes}
import aqua.res.{FuncRes, ServiceRes}

trait Types {
  def typed(field: String, `type`: String): String
  def generic(field: String, `type`: String): String
  def bang(field: String): String
  def funcType(f: FuncRes): FuncTypes
  def serviceType(s: ServiceRes): ServiceTypes
}

trait FuncTypes {
  def retTypeTs: (Option[String], String)
  def generate: String
}

trait ServiceTypes {
  def generate: String
}

object EmptyTypes extends Types {
  override def typed(field: String, `type`: String): String = field
  override def generic(field: String, `type`: String): String = field
  override def bang(field: String): String = field

  override def funcType(f: FuncRes): FuncTypes = new FuncTypes {
    override def retTypeTs: (Option[String], String) = (None, "")
    override def generate: String = ""
  }

  override def serviceType(s: ServiceRes): ServiceTypes = new ServiceTypes {
    override def generate: String = ""
  }
}
