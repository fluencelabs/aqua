package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.func.ServiceIdExpr
import aqua.parser.lexer.LiteralToken
import aqua.types.LiteralType
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ServiceIdExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec._

  "abilities" should "be parsed" in {
    parseAbId("Ab a") should be(
      ServiceIdExpr[Id](toNamedType("Ab"), toVar("a"))
    )

    parseAbId("Ab \"a\"") should be(
      ServiceIdExpr[Id](toNamedType("Ab"), LiteralToken[Id]("\"a\"", LiteralType.string))
    )

    parseAbId("Ab 1") should be(
      ServiceIdExpr[Id](toNamedType("Ab"), toNumber(1))
    )

    parseAbId("Ab a.id") should be(
      ServiceIdExpr[Id](toNamedType("Ab"), toVarLambda("a", List("id")))
    )
  }

}
