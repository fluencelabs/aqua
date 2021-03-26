package aqua.parser

import aqua.Utils
import aqua.parser.expr.{ReturnExpr, ServiceExpr}
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ServiceExprSpec extends AnyFlatSpec with Matchers with Utils {

  import Utils._

  "on" should "be parsed" in {
    parseService("service Local(\"local\")") should be(
      ServiceExpr[Id](toAb("Local"), Some(toStr("local")))
    )

    parseService("service Local(1)") should be(
      ServiceExpr[Id](toAb("Local"), Some(toNumber(1)))
    )

    parseService("service LocalBr") should be(
      ServiceExpr[Id](toAb("LocalBr"), None)
    )
  }
}
