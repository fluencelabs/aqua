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
    BasicTypeToken.`basictypedef`.parseAll("u32").value should be(u32: BasicTypeToken[Id])
    BasicTypeToken.`basictypedef`.parseAll("()").isLeft should be(true)
  }

  "Arrow type" should "parse" in {
    val arrowdef = ArrowTypeToken.`arrowdef`[Id](DataTypeToken.`datatypedef`[Id])
    val arrowWithNames = ArrowTypeToken.`arrowWithNames`[Id](DataTypeToken.`datatypedef`[Id])

    arrowdef.parseAll("-> B").value should be(
      ArrowTypeToken[Id]((), Nil, List(CustomTypeToken[Id]("B")))
    )
    arrowdef.parseAll("A -> B").value should be(
      ArrowTypeToken[Id](
        (),
        (None -> CustomTypeToken[Id]("A")) :: Nil,
        List(CustomTypeToken[Id]("B"))
      )
    )

    arrowWithNames.parseAll("(a: A) -> B").value should be(
      ArrowTypeToken[Id](
        (),
        (Some(Name[Id]("a")) -> CustomTypeToken[Id]("A")) :: Nil,
        List(CustomTypeToken[Id]("B"))
      )
    )

    arrowdef.parseAll("u32 -> Boo").value should be(
      ArrowTypeToken[Id](
        (),
        (None -> (u32: BasicTypeToken[Id])) :: Nil,
        List(CustomTypeToken[Id]("Boo"))
      )
    )
    TypeToken.`typedef`.parseAll("u32 -> ()").value should be(
      ArrowTypeToken[Id]((), (None -> (u32: BasicTypeToken[Id])) :: Nil, Nil)
    )
    arrowdef.parseAll("A, u32 -> B").value should be(
      ArrowTypeToken[Id](
        (),
        (None -> CustomTypeToken[Id]("A")) :: (None -> (u32: BasicTypeToken[Id])) :: Nil,
        List(CustomTypeToken[Id]("B"))
      )
    )
    arrowdef.parseAll("[]Absolutely, u32 -> B, C").value should be(
      ArrowTypeToken[Id](
        (),
        (Option.empty[Name[Id]] -> ArrayTypeToken[Id](
          (),
          CustomTypeToken[Id]("Absolutely")
        )) :: (Option.empty[Name[Id]] -> (u32: BasicTypeToken[
          Id
        ])) :: Nil,
        CustomTypeToken[Id]("B") ::
          CustomTypeToken[Id]("C") :: Nil
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
