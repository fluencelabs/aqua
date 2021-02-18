package aqua.parse

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeSpec extends AnyFlatSpec with Matchers{

  "Arrow type" should "parse" in {

    Type.`arrowdef`.parseAll("-> B") should be(Right(ArrowType(Nil, CustomType("B"))))
    Type.`arrowdef`.parseAll("A -> B") should be(Right(ArrowType(CustomType("A") :: Nil, CustomType("B"))))
    Type.`arrowdef`.parseAll("A, i32 -> B") should be(Right(ArrowType(CustomType("A") :: BasicType("i32") :: Nil, CustomType("B"))))
    Type.`arrowdef`.parseAll("[]Absolutely, i32 -> B") should be(Right(ArrowType(ArrayType(CustomType("Absolutely")) :: BasicType("i32") :: Nil, CustomType("B"))))

  }

  "Array type" should "parse" in {
    Type.`typedef`.parseAll("[]Something") should be(Right(ArrayType(CustomType("Something"))))
    Type.`typedef`.parseAll("[]i32") should be(Right(ArrayType(BasicType("i32"))))
    Type.`typedef`.parseAll("[][]i32") should be(Right(ArrayType(ArrayType(BasicType("i32")))))
  }

}
