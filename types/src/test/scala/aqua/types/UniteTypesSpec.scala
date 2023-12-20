package aqua.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.syntax.partialOrder._

class UniteTypesSpec extends AnyFlatSpec with Matchers {

  "unite types" should "work for scalars" ignore {

    ScalarType.i8 `∪` ScalarType.i16 should be(ScalarType.i16)
    ScalarType.i8 `∪` ScalarType.bool should be(TopType)
    ScalarType.i8 `∪` BottomType should be(ScalarType.i8)
    ScalarType.i8 `∪` TopType should be(TopType)
    ScalarType.i16 `∪` ScalarType.u32 should be(ScalarType.i64)

  }

  "unite types" should "work for collections" ignore {
    OptionType(ScalarType.i8) `∪` ArrayType(ScalarType.u16) should be(ArrayType(ScalarType.i32))
  }

  "unite types" should "work for products" ignore {
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
