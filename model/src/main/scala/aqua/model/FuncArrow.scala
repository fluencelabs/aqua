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

package aqua.model

import aqua.raw.arrow.FuncRaw
import aqua.raw.ops.{Call, CallArrowRawTag, RawTag}
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{ArrowType, MutableStreamType, Type}
import cats.syntax.option.*

case class FuncArrow(
  funcName: String,
  body: RawTag.Tree,
  arrowType: ArrowType,
  ret: List[ValueRaw],
  capturedArrows: Map[String, FuncArrow],
  capturedValues: Map[String, ValueModel],
  capturedTopology: Option[String]
) {

  lazy val args: List[(String, Type)] = arrowType.domain.toLabelledList()

  lazy val argNames: List[String] = args.map { case (name, _) => name }

  lazy val returnedArrows: Set[String] =
    ret.collect { case VarRaw(name, _: ArrowType) => name }.toSet

}

object FuncArrow {

  def fromRaw(
    raw: FuncRaw,
    arrows: Map[String, FuncArrow],
    constants: Map[String, ValueModel],
    topology: Option[String] = None
  ): FuncArrow =
    FuncArrow(
      raw.name,
      raw.arrow.body,
      raw.arrow.`type`,
      raw.arrow.ret,
      arrows,
      constants,
      topology
    )

  /**
   * Create function - wrapper around a service method
   *
   * @param funcName name of the function
   * @param serviceName name of the service
   * @param methodName name of the service method to wrap
   * @param methodType type of the service method to wrap
   * @param idValue resolved value of the service id
   * @return `FuncArrow` wrapper for the service method
   */
  def fromServiceMethod(
    funcName: String,
    serviceName: String,
    methodName: String,
    methodType: ArrowType,
    idValue: ValueModel | ValueRaw
  ): FuncArrow = {
    val (id, capturedValues) = idValue match {
      case i: ValueModel =>
        (
          VarRaw("id", i.`type`),
          Map("id" -> i)
        )
      case i: ValueRaw => (i, Map.empty[String, ValueModel])
    }
    val retVar = methodType.res.map(t => VarRaw("ret", t))

    val call = Call(
      methodType.domain.toLabelledList().map(VarRaw.apply),
      retVar.map { r =>
        Call.Export(r.name, r.`type`, Type.isStreamType(r.`type`))
      }.toList
    )

    val body = CallArrowRawTag.service(
      srvId = id,
      funcName = methodName,
      call = call,
      arrowType = methodType.some
    )

    FuncArrow(
      funcName = funcName,
      body = body.leaf,
      arrowType = methodType,
      ret = retVar.toList,
      capturedArrows = Map.empty,
      capturedValues = capturedValues,
      capturedTopology = None
    )
  }
}
