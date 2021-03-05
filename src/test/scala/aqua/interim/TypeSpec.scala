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
    accepts(u64, u32) should be(true)
    (u32: Type) <= u32 should be(true)
    (u32: Type) >= u32 should be(true)
    (u32: Type) > u32 should be(false)
    (u32: Type) > u64 should be(false)
    (u32: Type) > f64 should be(false)
    (u64: Type) > u32 should be(true)
    (u64: Type) >= string should be(false)
    (u64: Type) <= string should be(false)
  }

  "literal types" should "be accepted by scalars" in {
    accepts(u64, LiteralType.number) should be(true)
    accepts(bool, LiteralType.bool) should be(true)
    accepts(u32, LiteralType.bool) should be(false)
    accepts(f32, LiteralType.number) should be(true)
  }

  "arrays of scalars" should "be variant" in {
    (`[]`(u32): Type) <= u32 should be(false)
    (`[]`(u32): Type) >= u32 should be(false)
    (`[]`(u32): Type) <= `[]`(u32) should be(true)
    (`[]`(u32): Type) <= `[]`(u64) should be(true)
    (`[]`(u64): Type) <= `[]`(u32) should be(false)
    (`[]`(u64): Type) >= `[]`(u32) should be(true)
    (`[]`(u64): Type) > `[]`(u32) should be(true)
    (`[]`(u64): Type) >= `[]`(bool) should be(false)
    (`[]`(`[]`(u32)): Type) <= `[]`(u64) should be(false)
    (`[]`(`[]`(u32)): Type) <= `[]`(`[]`(u64)) should be(true)
  }

  "products of scalars" should "be variant" in {
    val one: Type = ProductType("one", NonEmptyMap.of("field" -> u32))
    val two: Type = ProductType("two", NonEmptyMap.of("field" -> u64, "other" -> string))
    val three: Type = ProductType("three", NonEmptyMap.of("field" -> u32))

    one < two should be(true)
    two > one should be(true)
    PartialOrder[Type].eqv(one, three) should be(true)
  }

  "arrows" should "be contravariant on arguments" in {
    val one: Type = ArrowType(u32 :: Nil, None)
    val two: Type = ArrowType(u64 :: Nil, None)

    accepts(one, two) should be(true)

    one > two should be(true)
    two < one should be(true)
  }

  "arrows" should "be variant on results" in {
    val one: Type = ArrowType(Nil, Some(u64))
    val two: Type = ArrowType(Nil, Some(u32))

    accepts(one, two) should be(true)

    one > two should be(true)
    two < one should be(true)
  }

  "arrows" should "respect both args and results" in {
    val one: Type = ArrowType(bool :: f64 :: Nil, Some(u64))
    val two: Type = ArrowType(bool :: Nil, Some(u64))
    val three: Type = ArrowType(bool :: f32 :: Nil, Some(u64))
    val four: Type = ArrowType(bool :: f32 :: Nil, Some(u32))

    accepts(one, two) should be(false)
    accepts(two, one) should be(false)

    accepts(one, three) should be(false)
    accepts(three, one) should be(true)

    accepts(one, four) should be(false)
    accepts(four, one) should be(false)
  }

}
