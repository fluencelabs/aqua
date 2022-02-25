package aqua.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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
  }

}
