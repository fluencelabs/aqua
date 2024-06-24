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

package aqua.raw.ops

import aqua.errors.Errors.internalError
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{ArrowType, ProductType, Type}

// TODO docs
case class Call(args: List[ValueRaw], exportTo: List[Call.Export]) {

  def mapValues(f: ValueRaw => ValueRaw): Call =
    Call(
      args.map(_.map(f)),
      exportTo
    )

  // TODO docs
  def mapExport(f: String => String): Call = copy(exportTo = exportTo.map(_.mapName(f)))

  def arrowType: ArrowType = ArrowType(
    ProductType(args.map(_.`type`)),
    ProductType.labelled(exportTo.map(e => e.name -> e.`type`))
  )

  override def toString: String =
    s"[${args.mkString(" ")}]${exportTo.map(_.toRaw).map(" " + _).mkString(",")}"
}

object Call {

  // TODO docs
  case class Export(name: String, `type`: Type, isExistingStream: Boolean = false) {
    def mapName(f: String => String): Export = copy(f(name))

    def mapStream(f: ValueRaw => ValueRaw): Call.Export =
      this match {
        // map streams from "exportTo", because they are not exports, but variables
        case ce @ Call.Export(_, _, true) =>
          f(ce.toRaw) match {
            case VarRaw(name, baseType) => Call.Export(name, baseType, true)
            case _ => internalError(s"Stream '$ce' can be only VarRaw")
          }
        case ce => ce
      }
      
    def renameNonStream(map: Map[String, String]): Call.Export =
      this match {
        case ce @ Call.Export(_, _, true) => ce
        case ce => ce.mapName(n => map.getOrElse(n, n))
      }

    def toRaw: VarRaw = VarRaw(name, `type`)

    override def toString: String = s"$name:${`type`} <-"
  }
}
