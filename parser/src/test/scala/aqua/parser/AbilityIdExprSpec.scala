package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.func.ServiceIdExpr
import aqua.parser.lexer.LiteralToken
import aqua.types.LiteralType

import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AbilityIdExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec._

  "abilities" should "be parsed" in {
    parseServiceId("Ab a") should be(
      ServiceIdExpr[Id](toNamedType("Ab"), toVar("a"))
    )

    parseServiceId("Ab \"a\"") should be(
      ServiceIdExpr[Id](toNamedType("Ab"), LiteralToken[Id]("\"a\"", LiteralType.string))
    )

    parseServiceId("Ab 1") should be(
      ServiceIdExpr[Id](toNamedType("Ab"), toNumber(1))
    )

    parseServiceId("Ab a.id") should be(
      ServiceIdExpr[Id](toNamedType("Ab"), toVarLambda("a", List("id")))
    )
  }

}
