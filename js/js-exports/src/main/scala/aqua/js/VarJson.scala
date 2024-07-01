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

package aqua.js

import aqua.raw.value.{ValueRaw, VarRaw}
import cats.data.Validated.{invalidNec, validNec}
import cats.data.ValidatedNec
import cats.syntax.traverse.*

import scala.scalajs.js

// Variable and its JSON value
case class VarJson(variable: VarRaw, value: js.Dynamic)

object VarJson {

  // checks if data is presented if there is non-literals in function arguments
  // creates services to add this data into a call
  def checkDataGetServices(
    args: List[ValueRaw] = Nil,
    data: Option[js.Dynamic]
  ): ValidatedNec[String, (List[ValueRaw], Map[String, VarJson])] = {
    val vars = args.collect { case v @ VarRaw(_, _) =>
      v
    // one variable could be used multiple times
    }.distinctBy(_.name)

    data match {
      case None if vars.nonEmpty =>
        // TODO: add a list  with actual argument names that where present in the function call
        invalidNec("Missing variables. You can provide them via --data or --data-path flags")
      case None =>
        validNec((args, Map.empty))
      case Some(data) =>
        vars.map { vm =>
          val arg = {
            val a = data.selectDynamic(vm.name)
            if (js.isUndefined(a)) null
            else a
          }

          val typeV = JsonEncoder.aquaTypeFromJson(vm.name, arg)

          typeV.map(t => (vm.copy(baseType = t), arg))
        }.sequence
          .map(_.map { case (vm, arg) =>
            vm.name -> VarJson(vm, arg)
          }.toMap)
          .andThen { services =>
            val argsWithTypes = args.map {
              case v @ VarRaw(n, _) =>
                // argument getters have been enriched with types derived from JSON
                // put this types to unriched arguments in CliFunc
                services.get(n).map(g => v.copy(baseType = g._1.baseType)).getOrElse(v)
              case v => v
            }

            validNec((argsWithTypes, services))
          }
    }
  }
}
