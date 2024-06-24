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

package aqua.raw.value

import aqua.types.{ArrowType, NilType, StructType, Type}
import cats.data.NonEmptyMap

sealed trait PropertyRaw {
  def `type`: Type

  /**
   * Apply function to values in this property
   */
  def map(f: ValueRaw => ValueRaw): PropertyRaw

  def renameVars(vals: Map[String, String]): PropertyRaw = this

  def varNames: Set[String]
}

case class IntoFieldRaw(name: String, `type`: Type) extends PropertyRaw {
  override def map(f: ValueRaw => ValueRaw): PropertyRaw = this

  override def varNames: Set[String] = Set.empty
}

case class IntoArrowRaw(name: String, arrowType: ArrowType, arguments: List[ValueRaw])
    extends PropertyRaw {

  override def `type`: Type = arrowType.codomain.uncons.map(_._1).getOrElse(NilType)

  override def map(f: ValueRaw => ValueRaw): PropertyRaw =
    copy(arguments = arguments.map(f))

  override def varNames: Set[String] = arguments.flatMap(_.varNames).toSet

  override def renameVars(vals: Map[String, String]): PropertyRaw =
    copy(arguments = arguments.map(_.renameVars(vals)))
}

case class IntoCopyRaw(`type`: StructType, fields: NonEmptyMap[String, ValueRaw])
    extends PropertyRaw {
  override def map(f: ValueRaw => ValueRaw): IntoCopyRaw = copy(fields = fields.map(f))

  override def varNames: Set[String] = Set.empty

  override def renameVars(vals: Map[String, String]): IntoCopyRaw = this
}

case class FunctorRaw(name: String, `type`: Type) extends PropertyRaw {
  override def map(f: ValueRaw => ValueRaw): FunctorRaw = this

  override def renameVars(vals: Map[String, String]): FunctorRaw = this

  override def varNames: Set[String] = Set.empty
}

case class IntoIndexRaw(idx: ValueRaw, `type`: Type) extends PropertyRaw {

  override def map(f: ValueRaw => ValueRaw): IntoIndexRaw = IntoIndexRaw(f(idx), `type`)

  override def renameVars(vals: Map[String, String]): IntoIndexRaw =
    IntoIndexRaw(idx.renameVars(vals), `type`)

  override def varNames: Set[String] = idx.varNames
}
