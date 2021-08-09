package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.ArrowTypeExpr
import aqua.parser.lexer.ArrowTypeToken
import aqua.types.ScalarType.{string, u32}
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArrowTypeExprSpec extends AnyFlatSpec with Matchers with AquaSpec {

  import AquaSpec._

  "arrow types" should "be parsed properly" in {
    parseArrow("onIn: string -> ()") should be(
      ArrowTypeExpr[Id]("onIn", toArrowType(List(string), None))
    )

    parseArrow("onIn: Custom -> Custom2") should be(
      ArrowTypeExpr[Id]("onIn", toArrowType(List("Custom"), Some("Custom2")))
    )

    parseArrow("onIn(a: Custom, b: Custom2)") should be(
      ArrowTypeExpr[Id](
        "onIn",
        toNamedArrow(List("a" -> toCustomType("Custom"), "b" -> toCustomType("Custom2")), Nil)
      )
    )

    parseArrow("onIn: Custom, string, u32, Custom3 -> Custom2") should be(
      ArrowTypeExpr[Id](
        "onIn",
        toArrowType(List("Custom", string, u32, "Custom3"), Some("Custom2"))
      )
    )
    parseArrow("onIn: Custom, string, u32, Custom3 -> Custom2, string") should be(
      ArrowTypeExpr[Id](
        "onIn",
        ArrowTypeToken[Id](
          (),
          List(toCustomType("Custom"), scToBt(string), scToBt(u32), toCustomType("Custom3"))
            .map(None -> _),
          List(toCustomType("Custom2"), scToBt(string))
        )
      )
    )
  }
}
