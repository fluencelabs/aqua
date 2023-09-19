package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.func.ForExpr
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ForExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec.{given, *}

  "for expression" should "be parsed" in {
    parseFor("for some <- \"a\"") should be(
      ForExpr[Id]("some", toStr("a"), None)
    )

    parseFor("for some <- \"a\"") should be(
      ForExpr[Id]("some", toStr("a"), None)
    )

    parseFor("for some <- 1") should be(
      ForExpr[Id]("some", toNumber(1), None)
    )

    parseFor("for some <- false") should be(
      ForExpr[Id]("some", toBool(false), None)
    )

    parseFor("for some <- false par") should be(
      ForExpr[Id]("some", toBool(false), Some(ForExpr.Mode.ParMode))
    )

    parseFor("for some <- false try") should be(
      ForExpr[Id]("some", toBool(false), Some(ForExpr.Mode.TryMode))
    )

  }
}
