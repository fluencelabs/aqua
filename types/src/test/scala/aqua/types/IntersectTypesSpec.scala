package aqua.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.data.NonEmptyMap
import cats.syntax.partialOrder._

class IntersectTypesSpec extends AnyFlatSpec with Matchers {

  "intersect types" should "work for scalars" in {

    ScalarType.i8 `∩` ScalarType.i16 should be(ScalarType.i8)
    ScalarType.i8 `∩` ScalarType.bool should be(BottomType)
    ScalarType.i8 `∩` BottomType should be(BottomType)
    ScalarType.i8 `∩` TopType should be(ScalarType.i8)
    ScalarType.i16 `∩` ScalarType.u32 should be(ScalarType.u8)
    ScalarType.i16 `∩` ScalarType.i32 should be(ScalarType.i16)

  }

  "intersect types" should "work for collections" in {
    OptionType(ScalarType.i8) `∩` ArrayType(ScalarType.u16) should be(OptionType(BottomType))
    OptionType(ScalarType.i16) `∩` ArrayType(ScalarType.u16) should be(OptionType(ScalarType.u8))
  }

  "intersect types" should "work for products" in {
    ProductType(ScalarType.i8 :: ScalarType.string :: Nil) `∩` ProductType(
      ScalarType.i8 :: Nil
    ) should be(ProductType(ScalarType.i8 :: Nil))

    ProductType(ScalarType.i8 :: ScalarType.string :: Nil) `∩` ProductType(
      ScalarType.i16 :: Nil
    ) should be(ProductType(ScalarType.i8 :: Nil))
  }

  "intersect types" should "work for structs" in {
    val x1: Type = StructType(
      "x1",
      NonEmptyMap.of[String, Type](
        "0" -> ScalarType.string,
        "1" -> ScalarType.u32,
        "2" -> ProductType(ScalarType.i8 :: ScalarType.string :: Nil)
      )
    )
    val x2: Type = StructType(
      "x2",
      NonEmptyMap.of[String, Type](
        "1" -> ScalarType.i16,
        "2" -> ProductType(ScalarType.i8 :: Nil),
        "3" -> ScalarType.bool
      )
    )

    val x1_x2: Type = StructType(
      "x1_x_x2",
      NonEmptyMap.of[String, Type](
        "1" -> ScalarType.u8,
        "2" -> ProductType(ScalarType.i8 :: Nil)
      )
    )

    x1 `∩` x2 should be(x1_x2)
  }

  "intersect types" should "work for arrows" in {
    val a1 = ArrowType(
      ProductType(
        ScalarType.i8 :: ScalarType.string :: Nil
      ),
      ProductType(ScalarType.bool :: Nil)
    )

    val a2 = ArrowType(
      ProductType(
        ScalarType.i16 :: Nil
      ),
      ProductType(ScalarType.bool :: ScalarType.string :: Nil)
    )

    val a1_a2 = ArrowType(
      ProductType(
        ScalarType.i8 :: ScalarType.string :: Nil
      ),
      ProductType(ScalarType.bool :: Nil)
    )

    a1 `∩` a2 should be(a1_a2)
  }

}
