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

import cats.syntax.partialOrder._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UniteTypesSpec extends AnyFlatSpec with Matchers {

  "unite types" should "work for scalars" in {

    ScalarType.i8 `∪` ScalarType.i16 should be(ScalarType.i16)
    ScalarType.i8 `∪` ScalarType.bool should be(TopType)
    ScalarType.i8 `∪` BottomType should be(ScalarType.i8)
    ScalarType.i8 `∪` TopType should be(TopType)
    ScalarType.i16 `∪` ScalarType.u32 should be(ScalarType.i64)

  }

  "unite types" should "work for collections" in {
    OptionType(ScalarType.i8) `∪` ArrayType(ScalarType.u16) should be(ArrayType(ScalarType.i32))
  }

  "unite types" should "work for products" in {
    val p1: Type = ProductType(
      ScalarType.i8 :: ScalarType.string :: Nil
    )

    val p2: Type = ProductType(
      ScalarType.i16 :: Nil
    )

    val p1_p2: Type = ProductType(
      ScalarType.i8 :: ScalarType.string :: Nil
    )

    p1.acceptsValueOf(p1_p2) should be(true)
    p2.acceptsValueOf(p1_p2) should be(true)
    p1 `∪` p2 should be(p1_p2)

  }

}
