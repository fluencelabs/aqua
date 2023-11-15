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
