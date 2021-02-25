package aqua.parse

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeSpec extends AnyFlatSpec with Matchers with EitherValues {

  "Basic type" should "parse" in {
    BasicType.`basictypedef`.parseAll("i32").right.value should be(BasicType("i32"))
    BasicType.`basictypedef`.parseAll("()").right.value should be(BasicType("()"))
  }

  "Arrow type" should "parse" in {

    Type.`arrowdef`.parseAll("-> B") should be(Right(ArrowType(Nil, CustomType("B"))))
    Type.`arrowdef`.parseAll("A -> B") should be(Right(ArrowType(CustomType("A") :: Nil, CustomType("B"))))
    Type.`arrowdef`.parseAll("i32 -> Boo") should be(Right(ArrowType(BasicType("i32") :: Nil, CustomType("Boo"))))
    Type.`typedef`.parseAll("i32 -> ()") should be(Right(ArrowType(BasicType("i32") :: Nil, BasicType("()"))))
    Type.`arrowdef`.parseAll("A, i32 -> B") should be(
      Right(ArrowType(CustomType("A") :: BasicType("i32") :: Nil, CustomType("B")))
    )
    Type.`arrowdef`.parseAll("[]Absolutely, i32 -> B") should be(
      Right(ArrowType(ArrayType(CustomType("Absolutely")) :: BasicType("i32") :: Nil, CustomType("B")))
    )

  }

  "Array type" should "parse" in {
    Type.`typedef`.parseAll("[]Something") should be(Right(ArrayType(CustomType("Something"))))
    Type.`typedef`.parseAll("[]i32") should be(Right(ArrayType(BasicType("i32"))))
    Type.`typedef`.parseAll("[][]i32") should be(Right(ArrayType(ArrayType(BasicType("i32")))))
  }

}
