package aqua.parser

import aqua.AquaSpec
import aqua.parser.lexer.{IntoArrow, PropertyOp, VarToken}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.Id

class IntoArrowSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec.*

  "into arrow" should "be parsed" in {
    val arrowStr = ".arrow(\"\")"

    val result = parseIntoArrow(arrowStr)
    result should be(IntoArrow[Id](toName("arrow"), toStr("") :: Nil))
  }

  "into arrow without arguments" should "be parsed" in {
    val arrowStr = ".arrow()"

    val result = parseIntoArrow(arrowStr)
    result should be(IntoArrow[Id](toName("arrow"), Nil))
  }

  "into arrow with value" should "be parsed" in {
    val arrowStr = "input.arrow(\"\")"

    val result = parseVar(arrowStr)
    val expected = VarToken[Id](toName("input"), IntoArrow[Id](toName("arrow"), toStr("") :: Nil) :: Nil)
    result should be(expected)
  }
}
