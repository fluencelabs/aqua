package aqua.parser

import aqua.Utils
import aqua.parser.expr.OnExpr
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OnExprSpec extends AnyFlatSpec with Matchers with Utils {
  import Utils._

  "on" should "be parsed" in {
    parseOn("on peer") should be(
      OnExpr[Id](toVar("peer", List()))
    )

    parseOn("on peer.id") should be(
      OnExpr[Id](toVar("peer", List("id")))
    )

    parseOn("on peer.id") should be(
      OnExpr[Id](toVar("peer", List("id")))
    )
  }
}
