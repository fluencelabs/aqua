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

package aqua.model.transform.pre

import aqua.raw.ops.{Call, CallArrowRawTag, RawTag}
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.Type

import cats.syntax.option.*

trait ResultsHandler {
  def handleResults(results: List[(String, Type)]): Option[RawTag.Tree]
}

case class CallbackResultsHandler(
  callbackSrvId: ValueRaw,
  funcName: String,
  noEmptyResponse: Boolean
) extends ResultsHandler {

  override def handleResults(results: List[(String, Type)]): Option[RawTag.Tree] =
    if (results.isEmpty && noEmptyResponse) none
    else {
      val resultVars = results.map(VarRaw.apply.tupled)
      val call = Call(
        args = resultVars,
        exportTo = Nil
      )

      CallArrowRawTag.service(callbackSrvId, funcName, call).leaf.some
    }
}
