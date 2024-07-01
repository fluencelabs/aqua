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

import aqua.backend.air.FuncAirGen
import aqua.backend.ts.TypeScriptCommon.fixupArgName
import aqua.backend.ts.{TSFuncTypes, TypeScriptCommon}
import aqua.definitions.*
import aqua.definitions.TypeDefinition.given
import aqua.res.FuncRes
import aqua.types.*

import cats.syntax.show.*
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*

case class OutputFunc(func: FuncRes, types: Types) {

  import FuncRes.*
  import TypeScriptCommon.*
  import func.*
  import types.*

  val funcTypes = types.funcType(func)

  import funcTypes.*

  def generate: (AirFunction, String) = {
    val tsAir = FuncAirGen(func).generate
    val codeLeftSpace = " " * 20

    val script = tsAir.show.linesIterator.map(codeLeftSpace + _).mkString("\n")
    val funcDef = FunctionDef(func)

    val scriptConstName = func.funcName + "_script"

    (
      AirFunction(func.funcName, script, funcDef),
      s"""export const $scriptConstName = `
         |$script
         |    `
         |${funcTypes.generate}
         |export function ${func.funcName}(${typed("...args", "any")}) {
         |

         |    return callFunction$$$$(
         |        args,
         |        ${funcDef.asJson.deepDropNullValues.spaces4},
         |        $scriptConstName
         |    )
         |}""".stripMargin
    )
  }

}
