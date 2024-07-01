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

import aqua.types.*

import cats.data.NonEmptyMap
import org.scalacheck.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scala.collection.immutable.SortedMap

class TypeVarianceSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {

  "ServiceType" should "be subtype of AbilityType" in {
    given Arbitrary[NonEmptyMap[String, ArrowType]] = Arbitrary(
      Gen
        .nonEmptyListOf(
          for {
            name <- anyName
            typ <- arrowOf(Arbitrary.arbitrary[DataType])
          } yield name -> typ
        )
        .map(m => NonEmptyMap.fromMapUnsafe(SortedMap.from(m)))
    )

    forAll {
      (
        arrows: NonEmptyMap[String, ArrowType],
        abName: String,
        srvName: String
      ) =>
        AbilityType(abName, arrows).acceptsValueOf(
          ServiceType(srvName, arrows)
        ) shouldBe true
    }
  }
}
