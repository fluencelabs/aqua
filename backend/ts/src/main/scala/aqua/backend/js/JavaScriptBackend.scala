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

package aqua.backend.js

import aqua.backend.ts.TypeScriptTypes
import aqua.backend.*
import aqua.res.AquaRes

case class JavaScriptBackend(isOldFluenceJs: Boolean = false, client: String = Backend.client) extends Backend {
  val types = TypeScriptTypes(client)

  def typesFile(res: AquaRes): Generated = {
    val services = res.services
      .map(s => types.serviceType(s))
      .map(_.generate)
      .toList
      .mkString("\n")
    val functions =
      res.funcs.map(f => types.funcType(f)).map(_.generate).toList.mkString("\n")

    val body = s"""/* eslint-disable */
                  |// @ts-nocheck
                  |${Header.header(true, isOldFluenceJs)}
                  |
                  |// Services
                  |$services
                  |
                  |// Functions
                  |$functions
                  |
                  |/* eslint-enable */""".stripMargin

    Generated(JavaScriptBackend.dtsExt, body, Nil)
  }

  override def generate(res: AquaRes): Seq[Generated] =
    if (res.isEmpty) Nil
    else {
      val (airs, script) = OutputFile(res).generate(EmptyTypes, true, isOldFluenceJs)
      Generated(JavaScriptBackend.ext, script, airs) :: typesFile(res) :: Nil
    }
}

object JavaScriptBackend {
  val ext = ".js"
  val dtsExt = ".d.ts"
}
