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

import aqua.backend.ServiceTypes
import aqua.backend.ts.TypeScriptCommon.fnDef
import aqua.res.ServiceRes

case class TSServiceTypes(srv: ServiceRes, client: String) extends ServiceTypes {

  val types = TypeScriptTypes(client)
  import types.*

  private val serviceTypeName = s"${srv.name}Def";

  private def registerServiceArgs = {

    // defined arguments used in overloads below
    val peerDecl = s"${typed("peer", client)}";
    val serviceIdDecl = s"${typed("serviceId", "string")}";
    val serviceDecl = s"${typed("service", serviceTypeName)}"

    // Service registration functions has several overloads.
    // Depending on whether the the service has the default id or not
    // there would be different number of overloads
    // This variable contain defines the list of lists where
    // the outmost list describes the list of overloads
    // and the innermost one defines the list of arguments in the overload
    val registerServiceArgsSource = srv.defaultId.fold(
      List(
        List(serviceIdDecl, serviceDecl),
        List(peerDecl, serviceIdDecl, serviceDecl)
      )
    )(_ =>
      List(
        List(serviceDecl),
        List(serviceIdDecl, serviceDecl),
        List(peerDecl, serviceDecl),
        List(peerDecl, serviceIdDecl, serviceDecl)
      )
    )

    // Service registration functions has several overloads.
    // Depending on whether the the service has the default id or not
    // there would be different number of overloads
    // This variable contain defines the list of lists where
    // the outmost list describes the list of overloads
    // and the innermost one defines the list of arguments in the overload
    registerServiceArgsSource.map { x =>
      val args = x.mkString(", ")
      s"export function register${srv.name}(${args}): void;"
    }.mkString("\n")
  }

  private def exportInterface = {
    val fnDefs = srv.members.map { case (name, arrow) =>
      s"    ${typed(name, fnDef(arrow))};"
    }
      .mkString("\n")

    s"""export interface ${serviceTypeName} {
       |${fnDefs}
       |}""".stripMargin
  }

  def generate: String = {
    s"""$exportInterface
       |$registerServiceArgs
       """
  }
}
