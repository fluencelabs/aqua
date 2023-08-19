package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.func.ParSeqExpr
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParSeqExprSpec extends AnyFlatSpec with Matchers with AquaSpec {

  import AquaSpec.*

  "parseq" should "be parsed" in {
    parseParSeq("parseq s <- strings on \"peerId\"") should be(
      ParSeqExpr[Id](toName("s"), toVar("strings"), toStr("peerId"), Nil)
    )

    parseParSeq("parseq s <- strings on \"peerId\" via \"relay\"") should be(
      ParSeqExpr[Id](toName("s"), toVar("strings"), toStr("peerId"), toStr("relay") :: Nil)
    )

    parseParSeq("parseq s <- strings on \"peerId\" via \"relay\" via \"relay2\"") should be(
      ParSeqExpr[Id](
        toName("s"),
        toVar("strings"),
        toStr("peerId"),
        toStr("relay") :: toStr("relay2") :: Nil
      )
    )

    parseParSeq("parseq s <- strings on peerId via relay") should be(
      ParSeqExpr[Id](toName("s"), toVar("strings"), toVar("peerId"), toVar("relay") :: Nil)
    )

  }
}
