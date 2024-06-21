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
import aqua.backend.{Header, OutputService}
import aqua.res.AquaRes

case class OutputFile(res: AquaRes) {

  def generate(types: Types, isJs: Boolean, isOldFluenceJs: Boolean): (List[AirFunction], String) = {
    import types.*
    val services = res.services
      .map(s => OutputService(s, types))
      .map(_.generate)
      .toList
      .mkString("\n\n")
    val scripts =
      res.funcs.map(f => OutputFunc(f, types)).map(_.generate)

    val (airs, functions) = scripts.toList.unzip

    (
      airs,
      s"""/* eslint-disable */
         |// @ts-nocheck
         |${Header.header(isJs, isOldFluenceJs)}
         |
         |// Services
         |$services
         |// Functions
         |${functions.mkString("\n\n")}
         |
         |/* eslint-enable */""".stripMargin
    )
  }

}
