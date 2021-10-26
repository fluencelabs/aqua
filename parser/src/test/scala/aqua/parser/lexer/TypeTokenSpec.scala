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

  import aqua.AquaSpec._

  implicit def strToBt(st: ScalarType): BasicTypeToken[Id] = BasicTypeToken[Id](st)

  "Basic type" should "parse" in {
    BasicTypeToken.`basictypedef`.parseAll("u32").value.mapK(spanToId) should be(strToBt(u32))
    BasicTypeToken.`basictypedef`.parseAll("()").isLeft should be(true)
  }

  "Arrow type" should "parse" in {
    def arrowdef(str: String) = ArrowTypeToken.`arrowdef`(DataTypeToken.`datatypedef`).parseAll(str).value.mapK(spanToId)
    def arrowWithNames(str: String) = ArrowTypeToken.`arrowWithNames`(DataTypeToken.`datatypedef`).parseAll(str).value.mapK(spanToId)

    arrowdef("-> B") should be(
      ArrowTypeToken[Id]((), Nil, List(CustomTypeToken[Id]("B")))
    )
    arrowdef("A -> B") should be(
      ArrowTypeToken[Id](
        (),
        (None -> CustomTypeToken[Id]("A")) :: Nil,
        List(CustomTypeToken[Id]("B"))
      )
    )

    arrowWithNames("(a: A) -> B") should be(
      ArrowTypeToken[Id](
        (),
        (Some(Name[Id]("a")) -> CustomTypeToken[Id]("A")) :: Nil,
        List(CustomTypeToken[Id]("B"))
      )
    )

    arrowdef("u32 -> Boo") should be(
      ArrowTypeToken[Id](
        (),
        (None -> strToBt(u32)) :: Nil,
        List(CustomTypeToken[Id]("Boo"))
      )
    )
    TypeToken.`typedef`.parseAll("u32 -> ()").value.mapK(spanToId) should be(
      ArrowTypeToken[Id]((), (None -> strToBt(u32)) :: Nil, Nil)
    )
    arrowdef("A, u32 -> B") should be(
      ArrowTypeToken[Id](
        (),
        (None -> CustomTypeToken[Id]("A")) :: (None -> strToBt(u32)) :: Nil,
        List(CustomTypeToken[Id]("B"))
      )
    )
    arrowdef("[]Absolutely, u32 -> B, C") should be(
      ArrowTypeToken[Id](
        (),
        (Option.empty[Name[Id]] -> ArrayTypeToken[Id]((), CustomTypeToken[Id]("Absolutely"))) ::
          (Option.empty[Name[Id]] -> strToBt(u32)) :: Nil,
        CustomTypeToken[Id]("B") ::
          CustomTypeToken[Id]("C") :: Nil
      )
    )

  }

  "Array type" should "parse" in {
    def typedef(str: String) = TypeToken.`typedef`.parseAll(str).value.mapK(spanToId)

    typedef("[]Something") should be(
      ArrayTypeToken[Id]((), CustomTypeToken[Id]("Something"))
    )
    typedef("[]u32") should be(
      ArrayTypeToken[Id]((), strToBt(u32))
    )
    typedef("[][]u32") should be(
      ArrayTypeToken[Id]((), ArrayTypeToken[Id]((), strToBt(u32)))
    )
  }

}
