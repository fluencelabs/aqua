package aqua.interim

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.syntax.partialOrder._
import Type.typesPartialOrder
import cats.data.NonEmptyMap

class TypeSpec extends AnyFlatSpec with Matchers {

  import ScalarType._

  def `[]`(t: DataType): DataType = ArrayType(t)

  "scalar types" should "be variant" in {
    (i32: Type) < i64 should be(true)
    (i32: Type) <= i32 should be(true)
    (i32: Type) >= i32 should be(true)
    (i32: Type) > i32 should be(false)
    (i32: Type) > i64 should be(false)
    (i32: Type) > f64 should be(false)
    (i64: Type) > i32 should be(true)
    (i64: Type) >= string should be(false)
    (i64: Type) <= string should be(false)
  }

  "arrays of scalars" should "be variant" in {
    (`[]`(i32): Type) <= i32 should be(false)
    (`[]`(i32): Type) >= i32 should be(false)
    (`[]`(i32): Type) <= `[]`(i32) should be(true)
    (`[]`(i32): Type) <= `[]`(i64) should be(true)
    (`[]`(i64): Type) <= `[]`(i32) should be(false)
    (`[]`(i64): Type) >= `[]`(i32) should be(true)
    (`[]`(i64): Type) > `[]`(i32) should be(true)
    (`[]`(i64): Type) >= `[]`(bool) should be(false)
    (`[]`(`[]`(i32)): Type) <= `[]`(i64) should be(false)
    (`[]`(`[]`(i32)): Type) <= `[]`(`[]`(i64)) should be(true)
  }

  "products of scalars" should "be variant" in {
    val one: Type = ProductType("one", NonEmptyMap.of("field" -> i32))
    val two: Type = ProductType("two", NonEmptyMap.of("field" -> i64, "other" -> string))

    one < two should be(true)
    two > one should be(true)
  }

  "arrows" should "be variant on arguments" in {
    val one: Type = ArrowType(i32 :: Nil, None)
    val two: Type = ArrowType(i64 :: Nil, None)

    one < two should be(true)
    two > one should be(true)
  }

  "arrows" should "be contravariant on results" in {
    val one: Type = ArrowType(Nil, Some(i64))
    val two: Type = ArrowType(Nil, Some(i32))

    one < two should be(true)
    two > one should be(true)
  }

}
