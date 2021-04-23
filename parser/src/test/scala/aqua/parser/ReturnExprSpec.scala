package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.ReturnExpr
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReturnExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec._

  "return" should "be parsed" in {
    parseReturn("<- true") should be(
      ReturnExpr[Id](toBool(true))
    )
  }
}
