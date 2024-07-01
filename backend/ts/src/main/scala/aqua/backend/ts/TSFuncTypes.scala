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

import aqua.backend.FuncTypes
import aqua.backend.ts.TypeScriptCommon.{fixupArgName, genTypeName, typeToTs}
import aqua.res.FuncRes
import aqua.types.*

case class TSFuncTypes(func: FuncRes, client: String) extends FuncTypes {
  val types = TypeScriptTypes(client) 
  import types.*

  override val retTypeTs =
    genTypeName(func.returnType, func.funcName.capitalize + "Result")

  override def generate = {
    val configType = "?: {ttl?: number}"

    val argsTypescript = func.args.map { arg =>
      val (typeDesc, t) =
        genTypeName(arg.`type`, func.funcName.capitalize + "Arg" + arg.name.capitalize)
      (typeDesc, s"${typed(fixupArgName(arg.name), t)}")
    } :+ (None, s"config$configType")

    val args = argsTypescript.map(a => "    " + a._2)
    val argsDesc = argsTypescript.flatMap(_._1)

    // defines different types for overloaded service registration function.
    val funcTypeOverload1 = args.mkString(",\n")
    val funcTypeOverload2 = (("    " + typed("peer", client)) :: args).mkString(",\n")

    val (resTypeDesc, resType) = retTypeTs

    s"""${argsDesc.mkString("\n")} 
       |${resTypeDesc.getOrElse("")}
       |export function ${func.funcName}(
       |${funcTypeOverload1}
       |): ${generic("Promise", resType)};
       |
       |export function ${func.funcName}(
       |${funcTypeOverload2}
       |): ${generic("Promise", resType)};
       |""".stripMargin
  }
}
