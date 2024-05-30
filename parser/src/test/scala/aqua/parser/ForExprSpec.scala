package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.func.ForExpr

import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ForExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec.{given, *}

  def forTestSuite(
    modeStr: String,
    mode: Option[ForExpr.Mode]
  ): Unit = {
    parseFor(s"for some <- 1$modeStr") should be(
      ForExpr[Id](Right("some"), toNumber(1), mode)
    )

    parseFor(s"for some <- false$modeStr") should be(
      ForExpr[Id](Right("some"), toBool(false), mode)
    )

    parseFor(s"for some <- \"a\"$modeStr") should be(
      ForExpr[Id](Right("some"), toStr("a"), mode)
    )

    parseFor(s"for i <- []$modeStr") should be(
      ForExpr[Id](Right("i"), toArr(Nil), mode)
    )

    parseFor(s"for i <- [1, 2, 3]$modeStr") should be(
      ForExpr[Id](Right("i"), toArr(List(toNumber(1), toNumber(2), toNumber(3))), mode)
    )

    parseFor(s"for i <- stream$modeStr") should be(
      ForExpr[Id](Right("i"), toVar("stream"), mode)
    )
  }

  "for expression" should "be parsed" in {
    forTestSuite("", None)
  }

  "for par expression" should "be parsed" in {
    forTestSuite(" par", Some(ForExpr.Mode.ParMode))
  }

  "for try expression" should "be parsed" in {
    forTestSuite(" try", Some(ForExpr.Mode.TryMode))
  }

  "for rec expression" should "be parsed" in {
    forTestSuite(" rec", Some(ForExpr.Mode.RecMode))
  }
}
