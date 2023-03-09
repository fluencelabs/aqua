package aqua.parser.lexer

import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.types.LiteralType
import cats.Id
import cats.data.{NonEmptyList, NonEmptyMap}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PropertyOpSpec extends AnyFlatSpec with Matchers with EitherValues {

  import aqua.AquaSpec._

  "lambda ops" should "parse" in {
    val opsP = (s: String) => PropertyOp.ops.parseAll(s).value.map(_.mapK(spanToId))

    opsP(".field") should be(NonEmptyList.of(IntoField[Id]("field")))
    opsP(".field.sub") should be(NonEmptyList.of(IntoField[Id]("field"), IntoField[Id]("sub")))
  }

  "index" should "parse" in {

    val idx = PropertyOp.ops.parseAll("[1]").value.map(_.mapK(spanToId)).head
    idx shouldBe IntoIndex[Id]((), Option(toNumber(1)))

    val idx2 = PropertyOp.ops.parseAll("[   1   ]").value.map(_.mapK(spanToId)).head
    idx2 shouldBe IntoIndex[Id]((), Option(toNumber(1)))

    val idx3 = PropertyOp.ops.parseAll(
      """[ -- comment1
        | -- comment2
        |   1 -- comment3
        |   -- comment4
        |]""".stripMargin).value.map(_.mapK(spanToId)).head
    idx3 shouldBe IntoIndex[Id]((), Option(toNumber(1)))

    PropertyOp.ops.parseAll("[-1]").isLeft shouldBe true
    PropertyOp.ops.parseAll("!-1").isLeft shouldBe true
  }

  "copy ops" should "parse" in {
    val opsP = (s: String) => PropertyOp.ops.parseAll(s).value.map(_.mapK(spanToId))

    opsP(".copy(a = \"str\", b = 12)") should be(
      NonEmptyList.of(
        IntoCopy[Id](
          (),
          NonEmptyMap.of(
            "a" -> LiteralToken("\"str\"", LiteralType.string),
            "b" -> LiteralToken("12", LiteralType.number)
          )
        )
      )
    )

    opsP(".copy(a = \"str\", b = 12).copy(c = 54, d = someVar)") should be(
      NonEmptyList.of(
        IntoCopy[Id](
          (),
          NonEmptyMap.of(
            "a" -> LiteralToken("\"str\"", LiteralType.string),
            "b" -> LiteralToken("12", LiteralType.number)
          )
        ),
        IntoCopy[Id](
          (),
          NonEmptyMap.of(
            "c" -> LiteralToken("54", LiteralType.number),
            "d" -> VarToken("someVar")
          )
        )
      )
    )
  }

}
