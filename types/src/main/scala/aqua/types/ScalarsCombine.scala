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
import cats.syntax.partialOrder.*

/**
 * Scalar types combiner for unions and intersections.
 * There are two combiners:
 * bot: X belongs to A and B (like intersection)
 * top: A and B belong to X (like union)
 */
object ScalarsCombine {
  type T = (ScalarType, ScalarType) => Type

  def bottom(a: ScalarType, b: ScalarType): Type =
    CompareTypes(a, b) match {
      case 1.0 => b
      case -1.0 => a
      case 0.0 => b
      case _ =>
        ScalarType.all
          .filter((_: Type) <= a)
          .filter((_: Type) <= b)
          .filter(x => (ScalarType.float(a) || ScalarType.float(b)) || !ScalarType.float(x))
          .filter(x => (ScalarType.signed(a) || ScalarType.signed(b)) || !ScalarType.signed(x))
          .toList
          .sortWith((_: Type) > _)
          .headOption
          .getOrElse(BottomType)
    }

  def top(a: ScalarType, b: ScalarType): Type =
    CompareTypes(a, b) match {
      case 1.0 => a
      case -1.0 => b
      case 0.0 => a
      case _ =>
        ScalarType.all
          .filter((_: Type) >= a)
          .filter((_: Type) >= b)
          .filter(x => (ScalarType.float(a) || ScalarType.float(b)) || !ScalarType.float(x))
          .filter(x => (ScalarType.signed(a) || ScalarType.signed(b)) || !ScalarType.signed(x))
          .toList
          .sortWith((_: Type) < _)
          .headOption
          .getOrElse(TopType)
    }

}
