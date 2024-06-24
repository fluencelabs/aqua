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

import aqua.backend.ts.TypeScriptCommon
import aqua.definitions.*
import aqua.definitions.TypeDefinition.given
import aqua.res.ServiceRes
import aqua.types.ArrowType

import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*

case class OutputService(srv: ServiceRes, types: Types) {

  import TypeScriptCommon.*
  import types.*
  private val serviceTypes = types.serviceType(srv)

  import serviceTypes.*

  def generate: String =
    val functions = LabeledProductTypeDef(
      srv.members.map { case (n, a) => (n, ArrowTypeDef(a)) }
    )

    val serviceDef = ServiceDef(srv.defaultId.map(s => s.replace("\"", "")), functions, srv.name)

    s"""
       |${serviceTypes.generate}
       |
       |export function register${srv.name}(${typed("...args", "any")}) {
       |    registerService$$$$(
       |        args,
       |        ${serviceDef.asJson.deepDropNullValues.spaces4}
       |    );
       |}
      """.stripMargin
}
