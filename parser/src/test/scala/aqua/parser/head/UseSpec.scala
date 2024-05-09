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

class UseSpec extends AnyFlatSpec with Matchers with AquaSpec {

  import AquaSpec.*

  "use" should "be parsed" in {
    parseUse("use DECLARE_CONST2 as DC2 from \"declare.aqua\" as Declare") shouldBe
      UseFromExpr(
        NonEmptyList.one(toQNameAs("DECLARE_CONST2", Some("DC2"))),
        toStr("declare.aqua"),
        Some(toQName("Declare"))
      )

    parseUse("use DECLARE_CONST from \"declare.aqua\" as Declare") shouldBe
      UseFromExpr(
        NonEmptyList.one(toQNameAs("DECLARE_CONST", None)),
        toStr("declare.aqua"),
        Some(toQName("Declare"))
      )
  }
}
