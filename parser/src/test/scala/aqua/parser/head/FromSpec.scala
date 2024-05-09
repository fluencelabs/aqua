package aqua.parser.head

import aqua.AquaSpec
import aqua.parser.expr.func.ServiceIdExpr
import aqua.parser.lexer.{LiteralToken, Token}
import aqua.parser.lift.LiftParser.given
import aqua.types.LiteralType

import cats.Id
import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FromSpec extends AnyFlatSpec with Matchers with AquaSpec {

  import AquaSpec.*

  "from constants" should "be parsed" in {
    parseQNameAs("SOME_CONSTANT") shouldBe toQNameAs("SOME_CONSTANT", None)
    parseQNameAs("SOME_CONSTANT as SC") shouldBe toQNameAs("SOME_CONSTANT", Some("SC"))
  }

  "from expression" should "be parsed" in {
    parseQNameAs("Ability") shouldBe toQNameAs("Ability", None)
    parseQNameAs("Ability as Ab") shouldBe toQNameAs("Ability", Some("Ab"))
    parseQNameAs("function") shouldBe toQNameAs("function", None)
    parseQNameAs("function as fn") shouldBe toQNameAs("function", Some("fn"))
  }

}
