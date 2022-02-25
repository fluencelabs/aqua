package aqua.types

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

}
