package aqua.parser.head

import aqua.AquaSpec
import aqua.parser.expr.func.AbilityIdExpr
import aqua.parser.lexer.{Literal, Token}
import aqua.parser.lift.LiftParser.Implicits.*
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
        NonEmptyList.one(Right((toAb("DECLARE_CONST2"), Some(toAb("DC2"))))),
        toStr("declare.aqua"),
        toAb("Declare")
      )

    parseUse("use DECLARE_CONST from \"declare.aqua\" as Declare") shouldBe
      UseFromExpr(
        NonEmptyList.one(Right((toAb("DECLARE_CONST"), None))),
        toStr("declare.aqua"),
        toAb("Declare")
      )
  }
}
