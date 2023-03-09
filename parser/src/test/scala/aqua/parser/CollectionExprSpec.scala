package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.AliasExpr
import aqua.parser.lexer.CollectionToken
import aqua.types.ScalarType.u32
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CollectionExprSpec extends AnyFlatSpec with Matchers with AquaSpec {

  import AquaSpec.*

  "collection" should "be parsed properly" in {
    parseCollection(
      """[
        |  "1",
        |"2"
        |]""".stripMargin) should be(
      CollectionToken[Id](CollectionToken.Mode.ArrayMode, List(toStr("1"), toStr("2")))
    )
  }
}
