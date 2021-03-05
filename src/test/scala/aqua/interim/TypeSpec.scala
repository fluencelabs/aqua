package aqua.interim

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.syntax.partialOrder._
import Type.typesPartialOrder
import cats.data.NonEmptyMap
import cats.kernel.PartialOrder

class TypeSpec extends AnyFlatSpec with Matchers {

  import ScalarType._

  def `[]`(t: DataType): DataType = ArrayType(t)

  def accepts(recv: Type, incoming: Type) =
    recv >= incoming

  "scalar types" should "be variant" in {
    accepts(i64, i32) should be(true)
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
    val three: Type = ProductType("three", NonEmptyMap.of("field" -> i32))

    one < two should be(true)
    two > one should be(true)
    PartialOrder[Type].eqv(one, three) should be(true)
  }

  "arrows" should "be contravariant on arguments" in {
    val one: Type = ArrowType(i32 :: Nil, None)
    val two: Type = ArrowType(i64 :: Nil, None)

    accepts(one, two) should be(true)

    one > two should be(true)
    two < one should be(true)
  }

  "arrows" should "be variant on results" in {
    val one: Type = ArrowType(Nil, Some(i64))
    val two: Type = ArrowType(Nil, Some(i32))

    accepts(one, two) should be(true)

    one > two should be(true)
    two < one should be(true)
  }

  "arrows" should "respect both args and results" in {
    val one: Type = ArrowType(bool :: f64 :: Nil, Some(i64))
    val two: Type = ArrowType(bool :: Nil, Some(i64))
    val three: Type = ArrowType(bool :: f32 :: Nil, Some(i64))
    val four: Type = ArrowType(bool :: f32 :: Nil, Some(i32))

    accepts(one, two) should be(false)
    accepts(two, one) should be(false)

    accepts(one, three) should be(false)
    accepts(three, one) should be(true)

    accepts(one, four) should be(false)
    accepts(four, one) should be(false)
  }

}
