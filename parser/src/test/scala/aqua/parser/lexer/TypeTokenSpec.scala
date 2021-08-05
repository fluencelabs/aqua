package aqua.parser.lexer

import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.types.ScalarType
import aqua.types.ScalarType.u32
import cats.Id
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class TypeTokenSpec extends AnyFlatSpec with Matchers with EitherValues {

  implicit def strToBt(st: ScalarType): BasicTypeToken[Id] = BasicTypeToken[Id](st)

  "Basic type" should "parse" in {
    BasicTypeToken.`basictypedef`.parseAll("u32").right.value should be(u32: BasicTypeToken[Id])
    BasicTypeToken.`basictypedef`.parseAll("()") should be("left")
  }

  "Arrow type" should "parse" in {

    ArrowTypeToken.`arrowdef`.parseAll("-> B").right.value should be(
      ArrowTypeToken[Id]((), Nil, Some(CustomTypeToken[Id]("B")))
    )
    ArrowTypeToken.`arrowdef`.parseAll("A -> B").right.value should be(
      ArrowTypeToken[Id]((), CustomTypeToken[Id]("A") :: Nil, Some(CustomTypeToken[Id]("B")))
    )

    ArrowTypeToken.`arrowWithNames`.parseAll("(a: A) -> B").right.value should be(
      ArrowTypeToken[Id]((), CustomTypeToken[Id]("A") :: Nil, Some(CustomTypeToken[Id]("B")))
    )

    ArrowTypeToken.`arrowdef`.parseAll("u32 -> Boo").right.value should be(
      ArrowTypeToken[Id]((), (u32: BasicTypeToken[Id]) :: Nil, Some(CustomTypeToken[Id]("Boo")))
    )
    TypeToken.`typedef`.parseAll("u32 -> ()").right.value should be(
      ArrowTypeToken[Id]((), (u32: BasicTypeToken[Id]) :: Nil, None)
    )
    ArrowTypeToken.`arrowdef`.parseAll("A, u32 -> B").right.value should be(
      ArrowTypeToken[Id](
        (),
        CustomTypeToken[Id]("A") :: (u32: BasicTypeToken[Id]) :: Nil,
        Some(CustomTypeToken[Id]("B"))
      )
    )
    ArrowTypeToken.`arrowdef`.parseAll("[]Absolutely, u32 -> B").right.value should be(
      ArrowTypeToken[Id](
        (),
        ArrayTypeToken[Id]((), CustomTypeToken[Id]("Absolutely")) :: (u32: BasicTypeToken[
          Id
        ]) :: Nil,
        Some(CustomTypeToken[Id]("B"))
      )
    )

  }

  "Array type" should "parse" in {
    TypeToken.`typedef`.parseAll("[]Something") should be(
      Right(ArrayTypeToken[Id]((), CustomTypeToken[Id]("Something")))
    )
    TypeToken.`typedef`.parseAll("[]u32") should be(
      Right(ArrayTypeToken[Id]((), u32: BasicTypeToken[Id]))
    )
    TypeToken.`typedef`.parseAll("[][]u32") should be(
      Right(ArrayTypeToken[Id]((), ArrayTypeToken[Id]((), u32: BasicTypeToken[Id])))
    )
  }

}
