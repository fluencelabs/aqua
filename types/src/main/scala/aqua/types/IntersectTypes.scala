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

package aqua.types

import aqua.errors.Errors.internalError

import cats.Monoid
import cats.data.NonEmptyMap

/**
 * Intersection of types makes a new type that can be found in both arguments
 *
 * X := Intersection of A, B =>
 * X belongs to A
 * X belongs to B
 */
case class IntersectTypes(scalarsCombine: ScalarsCombine.T) extends Monoid[Type]:

  override def empty: Type = TopType

  def combineProducts(ap: ProductType, bp: ProductType): ProductType =
    ProductType(
      ap.toList.zip(bp.toList).map(combine)
    )

  private def combineDataTypes(a: DataType, b: DataType): DataType =
    (a `∩` b) match {
      case d: DataType => d
      case t => internalError(s"$a ∩ $b yields non-data type $t")
    }

  override def combine(a: Type, b: Type): Type =
    (a, b) match {
      case _ if CompareTypes(a, b) == 0.0 => a

      case (ap: ProductType, bp: ProductType) =>
        combineProducts(ap, bp)

      case (as: StructType, bs: StructType) =>
        NonEmptyMap
          .fromMap(as.fields.toSortedMap.flatMap { case (ak, at) =>
            bs.fields.lookup(ak).map(bt => ak -> combine(at, bt))
          })
          .fold(empty)(fields => StructType(s"${as.name}_x_${bs.name}", fields))

      case (aa: ArrowType, bb: ArrowType) =>
        ArrowType(
          UniteTypes.top.combineProducts(aa.domain, bb.domain),
          combineProducts(aa.codomain, bb.codomain)
        )

      case (ac: OptionType, bc: CollectionType) =>
        OptionType(combineDataTypes(ac.element, bc.element))
      case (ac: CollectionType, bc: OptionType) =>
        OptionType(combineDataTypes(ac.element, bc.element))

      case (ac: ArrayType, bc: CollectionType) =>
        ArrayType(combineDataTypes(ac.element, bc.element))
      case (ac: CollectionType, bc: ArrayType) =>
        ArrayType(combineDataTypes(ac.element, bc.element))

      case (ac: StreamType, bc: StreamType) =>
        StreamType(combineDataTypes(ac.element, bc.element))
      case (ac: StreamMapType, bc: StreamMapType) =>
        StreamMapType(combineDataTypes(ac.element, bc.element))

      case (a: ScalarType, b: ScalarType) =>
        scalarsCombine(a, b)

      case _ =>
        CompareTypes.apply(a, b) match {
          case 1.0 => b
          case -1.0 => a
          case 0.0 => b
          case _ =>
            BottomType
        }
    }

object IntersectTypes:
  val top: IntersectTypes = IntersectTypes(ScalarsCombine.top)
  val bottom: IntersectTypes = IntersectTypes(ScalarsCombine.bottom)
