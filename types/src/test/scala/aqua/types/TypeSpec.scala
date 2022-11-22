package aqua.types

import aqua.types.Type.typesPartialOrder
import cats.data.{NonEmptyMap, NonEmptyList}
import cats.kernel.PartialOrder
import cats.syntax.partialOrder._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeSpec extends AnyFlatSpec with Matchers {

  import aqua.types.ScalarType._

  def `[]`(t: DataType): DataType = ArrayType(t)

  def `?`(t: DataType): DataType = OptionType(t)

  def `*`(t: DataType): DataType = StreamType(t)

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

  "top type" should "accept anything" in {
    accepts(TopType, u64) should be(true)
    accepts(TopType, LiteralType.bool) should be(true)
    accepts(TopType, `*`(u64)) should be(true)
  }

  "bottom type" should "be accepted by everything" in {
    accepts(u64, BottomType) should be(true)
    accepts(LiteralType.bool, BottomType) should be(true)
    accepts(`*`(u64), BottomType) should be(true)
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

  "structs of scalars" should "be variant" in {
    val one: Type = StructType("one", NonEmptyList.of("field" -> u32))
    val two: Type = StructType("two", NonEmptyList.of("field" -> u64, "other" -> string))
    val three: Type = StructType("three", NonEmptyList.of("field" -> u32))

    accepts(one, two) should be(true)
    accepts(two, one) should be(false)
    PartialOrder[Type].eqv(one, three) should be(true)
  }

  "streams" should "be accepted as an array, but not vice versa" in {
    val stream: Type = StreamType(bool)
    val array: Type = ArrayType(bool)

    accepts(array, stream) should be(true)
    accepts(stream, array) should be(false)
    accepts(stream, stream) should be(true)
  }

  "streams" should "be accepted as an option, but not vice versa" in {
    val stream: Type = StreamType(bool)
    val opt: Type = OptionType(bool)

    accepts(opt, stream) should be(true)
    accepts(stream, opt) should be(false)
    accepts(opt, opt) should be(true)
  }

  "products" should "compare" in {
    val empty: ProductType = NilType
    val smth: ProductType = ConsType.cons(bool, empty)

    accepts(empty, smth) should be(true)
    accepts(smth, empty) should be(false)

    val longer = ConsType.cons(string, smth)
    accepts(empty, longer) should be(true)
    accepts(smth, longer) should be(false)
    accepts(longer, longer) should be(true)
    accepts(longer, empty) should be(false)
    accepts(longer, smth) should be(false)
    accepts(ConsType.cons("label", string, empty), longer) should be(true)

    accepts(ConsType.cons(u64, empty), ConsType.cons(u32, empty)) should be(true)
    accepts(ConsType.cons(u32, empty), ConsType.cons(u64, empty)) should be(false)

    def p(types: Type*): Type = ProductType(types.toList)

    p(u32) < p(u64) should be(true)
    p(u32) < p(u64, string) should be(false)
    p(u64).acceptsValueOf(p(u16, string)) should be(true)
  }

  "arrows" should "be contravariant on arguments" in {
    val one: Type = ArrowType(ProductType(u32 :: Nil), NilType)
    val onePrime: Type = ArrowType(ProductType(u32 :: bool :: Nil), NilType)
    val two: Type = ArrowType(ProductType(u64 :: Nil), NilType)

    accepts(one, onePrime) should be(false)
    accepts(onePrime, one) should be(true)
    accepts(one, two) should be(true)
    accepts(onePrime, two) should be(true)

    one > two should be(true)
    two < one should be(true)
  }

  "arrows" should "be variant on results" in {
    val one: Type = ArrowType(NilType, ProductType(u64 :: Nil))
    val two: Type = ArrowType(NilType, ProductType(u32 :: Nil))
    val three: Type = ArrowType(NilType, ProductType(u32 :: bool :: Nil))

    accepts(one, two) should be(true)
    accepts(one, three) should be(true)
    accepts(three, two) should be(false)
    accepts(three, one) should be(false)
    accepts(two, one) should be(false)

    one > two should be(true)
    two < one should be(true)
  }

  "arrows" should "respect both args and results" in {
    val one: Type = ArrowType(ProductType(bool :: f64 :: Nil), ProductType(u64 :: Nil))
    val two: Type = ArrowType(ProductType(bool :: Nil), ProductType(u64 :: Nil))
    val three: Type = ArrowType(ProductType(bool :: f32 :: Nil), ProductType(u64 :: Nil))
    val four: Type = ArrowType(ProductType(bool :: f32 :: Nil), ProductType(u32 :: Nil))

    accepts(one, two) should be(true)
    accepts(two, one) should be(false)

    accepts(one, three) should be(false)
    accepts(three, one) should be(true)

    accepts(one, four) should be(false)
    accepts(four, one) should be(false)
  }

  "labeled types" should "create correc labels" in {
    val cons = LabeledConsType(
      "arg1",
      ArrowType(
        UnlabeledConsType(string, NilType),
        UnlabeledConsType(string, NilType)
      ),
      LabeledConsType("arg2", string, NilType)
    )

    cons.labelledData should be(("arg2", string) :: Nil)
  }

}
