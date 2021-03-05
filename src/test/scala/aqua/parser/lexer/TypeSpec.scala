package aqua.parser.lexer

import aqua.interim.ScalarType
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.Id

import scala.language.implicitConversions

class TypeSpec extends AnyFlatSpec with Matchers with EitherValues {

  import aqua.interim.ScalarType.i32

  implicit def strToBt(st: ScalarType): BasicTypeToken[Id] = BasicTypeToken[Id](st)

  "Basic type" should "parse" in {
    BasicTypeToken.`basictypedef`.parseAll("i32").right.value should be(i32: BasicTypeToken[Id])
    BasicTypeToken.`basictypedef`.parseAll("()") should be('left)
  }

  "Arrow type" should "parse" in {

    TypeToken.`arrowdef`.parseAll("-> B").right.value should be(
      ArrowTypeToken[Id]((), Nil, Some(CustomTypeToken[Id]("B")))
    )
    TypeToken.`arrowdef`.parseAll("A -> B").right.value should be(
      ArrowTypeToken[Id]((), CustomTypeToken[Id]("A") :: Nil, Some(CustomTypeToken[Id]("B")))
    )
    TypeToken.`arrowdef`.parseAll("i32 -> Boo").right.value should be(
      ArrowTypeToken[Id]((), (i32: BasicTypeToken[Id]) :: Nil, Some(CustomTypeToken[Id]("Boo")))
    )
    TypeToken.`typedef`.parseAll("i32 -> ()").right.value should be(
      ArrowTypeToken[Id]((), (i32: BasicTypeToken[Id]) :: Nil, None)
    )
    TypeToken.`arrowdef`.parseAll("A, i32 -> B").right.value should be(
      ArrowTypeToken[Id](
        (),
        CustomTypeToken[Id]("A") :: (i32: BasicTypeToken[Id]) :: Nil,
        Some(CustomTypeToken[Id]("B"))
      )
    )
    TypeToken.`arrowdef`.parseAll("[]Absolutely, i32 -> B").right.value should be(
      ArrowTypeToken[Id](
        (),
        ArrayTypeToken(CustomTypeToken[Id]("Absolutely")) :: (i32: BasicTypeToken[Id]) :: Nil,
        Some(CustomTypeToken[Id]("B"))
      )
    )

  }

  "Array type" should "parse" in {
    TypeToken.`typedef`.parseAll("[]Something") should be(Right(ArrayTypeToken(CustomTypeToken[Id]("Something"))))
    TypeToken.`typedef`.parseAll("[]i32") should be(Right(ArrayTypeToken(i32: BasicTypeToken[Id])))
    TypeToken.`typedef`.parseAll("[][]i32") should be(
      Right(ArrayTypeToken[Id](ArrayTypeToken[Id](i32: BasicTypeToken[Id])))
    )
  }

}
