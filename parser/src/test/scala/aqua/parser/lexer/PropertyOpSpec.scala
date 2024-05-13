package aqua.parser.lexer

import aqua.parser.lift.LiftParser.given
import aqua.types.LiteralType

import cats.Id
import cats.data.{NonEmptyList, NonEmptyMap}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PropertyOpSpec extends AnyFlatSpec with Matchers with EitherValues {

  import aqua.AquaSpec.{given, *}

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

    val idx3 = PropertyOp.ops
      .parseAll("""[ -- comment1
                  | -- comment2
                  |   1 -- comment3
                  |   -- comment4
                  |]""".stripMargin)
      .value
      .map(_.mapK(spanToId))
      .head
    idx3 shouldBe IntoIndex[Id]((), Option(toNumber(1)))

    PropertyOp.ops.parseAll("[-1]").isLeft shouldBe true
    PropertyOp.ops.parseAll("!-1").isLeft shouldBe true
  }

  def copyOpsP(s: String) = PropertyOp.ops.parseAll(s).value.map(_.mapK(spanToId))

  "copy ops" should "parse one copy" in {
    copyOpsP(".copy(a = \"str\", b = 12)") should be(
      NonEmptyList.of(
        IntoCopy[Id](
          (),
          NonEmptyList.of(
            NamedArg.Full(toName("a"), toStr("str")),
            NamedArg.Full(toName("b"), toNumber(12))
          )
        )
      )
    )
  }

  it should "parse sequential copy" in {
    copyOpsP(".copy(a = \"str\", b = 12).copy(c = 54, d = someVar)") should be(
      NonEmptyList.of(
        IntoCopy[Id](
          (),
          NonEmptyList.of(
            NamedArg.Full(toName("a"), toStr("str")),
            NamedArg.Full(toName("b"), toNumber(12))
          )
        ),
        IntoCopy[Id](
          (),
          NonEmptyList.of(
            NamedArg.Full(toName("c"), toNumber(54)),
            NamedArg.Full(toName("d"), toVar("someVar"))
          )
        )
      )
    )
  }

  it should "parse mixed args in copy" in {
    val args = List(
      "a = \"str\"" -> NamedArg.Full(toName("a"), toStr("str")),
      "b = 12" -> NamedArg.Full(toName("b"), toNumber(12)),
      "c" -> NamedArg.Short(toVar("c")),
      "d" -> NamedArg.Short(toVar("d"))
    )

    args.toSet.subsets().filter(_.nonEmpty).flatMap(_.toList.permutations).foreach { args =>
      val str = args.map(_._1).mkString(".copy(", ", ", ")")
      val expected = NonEmptyList.of(
        IntoCopy[Id]((), NonEmptyList.fromListUnsafe(args.map(_._2)))
      )

      copyOpsP(str) should be(expected)
    }
  }

}
