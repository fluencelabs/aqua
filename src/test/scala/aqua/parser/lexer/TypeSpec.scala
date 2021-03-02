package aqua.parser.lexer

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.Id
import scala.language.implicitConversions

class TypeSpec extends AnyFlatSpec with Matchers with EitherValues {

  implicit def strToBt(str: String): BasicType[Id] = BasicType[Id](BasicType.Value(str))

  "Basic type" should "parse" in {
    BasicType.`basictypedef`.parseAll("i32").right.value should be("i32": BasicType[Id])
    BasicType.`basictypedef`.parseAll("()").right.value should be("()": BasicType[Id])
  }

  "Arrow type" should "parse" in {

    Type.`arrowdef`.parseAll("-> B") should be(Right(ArrowType(Nil, CustomType[Id]("B"))))
    Type.`arrowdef`.parseAll("A -> B") should be(Right(ArrowType(CustomType[Id]("A") :: Nil, CustomType[Id]("B"))))
    Type.`arrowdef`.parseAll("i32 -> Boo") should be(
      Right(ArrowType(("i32": BasicType[Id]) :: Nil, CustomType[Id]("Boo")))
    )
    Type.`typedef`.parseAll("i32 -> ()") should be(Right(ArrowType(("i32": BasicType[Id]) :: Nil, "()")))
    Type.`arrowdef`.parseAll("A, i32 -> B") should be(
      Right(ArrowType(CustomType[Id]("A") :: ("i32": BasicType[Id]) :: Nil, CustomType[Id]("B")))
    )
    Type.`arrowdef`.parseAll("[]Absolutely, i32 -> B") should be(
      Right(ArrowType(ArrayType(CustomType[Id]("Absolutely")) :: ("i32": BasicType[Id]) :: Nil, CustomType[Id]("B")))
    )

  }

  "Array type" should "parse" in {
    Type.`typedef`.parseAll("[]Something") should be(Right(ArrayType(CustomType[Id]("Something"))))
    Type.`typedef`.parseAll("[]i32") should be(Right(ArrayType("i32": BasicType[Id])))
    Type.`typedef`.parseAll("[][]i32") should be(Right(ArrayType[Id](ArrayType[Id]("i32": BasicType[Id]))))
  }

}
