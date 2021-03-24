package aqua.parser

import aqua.Utils
import aqua.parser.expr.AbilityIdExpr
import aqua.parser.lexer.Literal
import aqua.semantics.LiteralType
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AbilitIdExprSpec extends AnyFlatSpec with Matchers with Utils {
  import Utils._

  "abilities" should "be parsed" in {
    parseAbId("Ab a") should be(
      AbilityIdExpr[Id](toAb("Ab"), toVar("a", List()))
    )

    parseAbId("Ab \"a\"") should be(
      AbilityIdExpr[Id](toAb("Ab"), Literal[Id]("\"a\"", LiteralType.string))
    )

    parseAbId("Ab 1") should be(
      AbilityIdExpr[Id](toAb("Ab"), Literal[Id]("1", LiteralType.number))
    )

    parseAbId("Ab a.id") should be(
      AbilityIdExpr[Id](toAb("Ab"), toVar("a", List("id")))
    )
  }

}
