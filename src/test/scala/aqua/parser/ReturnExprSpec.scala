package aqua.parser

import aqua.Utils
import aqua.parser.expr.{ParExpr, ReturnExpr}
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReturnExprSpec extends AnyFlatSpec with Matchers with Utils {
  import Utils._

  "on" should "be parsed" in {
    parseReturn("<- true") should be(
      ReturnExpr[Id](toBool(true))
    )
  }
}
