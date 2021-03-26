package aqua.parser

import aqua.Utils
import aqua.parser.expr.OnExpr
import aqua.semantics.LiteralType.{number, string}
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OnExprSpec extends AnyFlatSpec with Matchers with Utils {
  import Utils._

  "on" should "be parsed" in {
    parseOn("on peer") should be(
      OnExpr[Id](toVar("peer"), Nil)
    )

    parseOn("on peer.id") should be(
      OnExpr[Id](toVarLambda("peer", List("id")), Nil)
    )

    parseOn("on \"peer\"") should be(
      OnExpr[Id](toStr("peer"), Nil)
    )

    parseOn("on 1") should be(
      OnExpr[Id](toNumber(1), Nil)
    )
  }
}
