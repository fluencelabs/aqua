package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.AliasExpr
import aqua.types.ScalarType.u32
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AliasExprSpec extends AnyFlatSpec with Matchers with AquaSpec {

  import AquaSpec.{given, *}

  "alias" should "be parsed properly" in {
    parseAlias("alias SomeAlias : u32") should be(
      AliasExpr[Id]("SomeAlias", u32)
    )

    parseAlias("alias SomeAlias : CustomType") should be(
      AliasExpr[Id]("SomeAlias", "CustomType")
    )
  }

  "alias" should "be parsed without spaces" in {
    parseAlias("alias SomeAlias:CustomType") should be(
      AliasExpr[Id]("SomeAlias", "CustomType")
    )
  }
}
