package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.{OnExpr, ParExpr}
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec._

  "on" should "be parsed" in {
    parsePar("par") should be(
      ParExpr[Id](())
    )
  }
}
