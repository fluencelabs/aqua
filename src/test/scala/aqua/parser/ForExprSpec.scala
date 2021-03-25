package aqua.parser

import aqua.Utils
import aqua.parser.expr.ForExpr
import aqua.semantics.LiteralType.{bool, number, string}
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ForExprSpec extends AnyFlatSpec with Matchers with Utils {
  import Utils._

  "on" should "be parsed" in {
    parseFor("for some <- \"a\"") should be(
      ForExpr[Id]("some", toLiteral("\"a\"", string))
    )

    parseFor("for some <- \"a\"") should be(
      ForExpr[Id]("some", toLiteral("\"a\"", string))
    )

    parseFor("for some <- 1") should be(
      ForExpr[Id]("some", toLiteral("1", number))
    )

    parseFor("for some <- false") should be(
      ForExpr[Id]("some", toLiteral("false", bool))
    )
  }
}
