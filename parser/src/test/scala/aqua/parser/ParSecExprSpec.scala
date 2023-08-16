package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.func.ParSecExpr
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParSecExprSpec extends AnyFlatSpec with Matchers with AquaSpec {

  import AquaSpec.*

  "parsec" should "be parsed" in {
    parseParSec("parsec s <- strings on \"peerId\"") should be(
      ParSecExpr[Id](toName("s"), toVar("strings"), toStr("peerId"), Nil)
    )

    parseParSec("parsec s <- strings on \"peerId\" via \"relay\"") should be(
      ParSecExpr[Id](toName("s"), toVar("strings"), toStr("peerId"), toStr("relay") :: Nil)
    )

    parseParSec("parsec s <- strings on \"peerId\" via \"relay\" via \"relay2\"") should be(
      ParSecExpr[Id](
        toName("s"),
        toVar("strings"),
        toStr("peerId"),
        toStr("relay") :: toStr("relay2") :: Nil
      )
    )

    parseParSec("parsec s <- strings on peerId via relay") should be(
      ParSecExpr[Id](toName("s"), toVar("strings"), toVar("peerId"), toVar("relay") :: Nil)
    )

  }
}
