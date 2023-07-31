package aqua.parser

import aqua.AquaSpec
import aqua.AquaSpec.{toNumber, toStr, toVar}
import aqua.parser.expr.ConstantExpr
import aqua.parser.expr.func.AssignmentExpr
import aqua.parser.lexer.CollectionToken.Mode.ArrayMode
import aqua.parser.lexer.*
import aqua.types.LiteralType
import cats.Id
import cats.data.{NonEmptyList, NonEmptyMap}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AbilityValueExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec.*

  private def parseAndCheckAbility(str: String) = {
    parseData(
      str
    ) should be(
      NamedValueToken(
        NamedTypeToken[Id]("AbilityA"),
        NonEmptyMap.of(
          "v1" -> toNumber(1),
          "f1" -> VarToken(Name[Id]("input"), IntoField[Id]("arrow") :: Nil)
        )
      )
    )
  }

  "one line struct value" should "be parsed" in {
    parseAndCheckAbility("""AbilityA(v1 = 1, f1 = input.arrow)""")
  }

  "multiline line struct value" should "be parsed" in {
    parseAndCheckAbility("""AbilityA(v1 = 1, f1 = input.arrow)""".stripMargin)
  }

}
