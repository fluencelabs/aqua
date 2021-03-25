package aqua.parser

import aqua.Utils
import aqua.parser.expr.{ElseOtherwiseExpr, OnExpr}
import aqua.semantics.LiteralType.{number, string}
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ElseOtherwiseExprSpec extends AnyFlatSpec with Matchers with Utils {
  import Utils._

  "else" should "be parsed" in {
    parseElse("else") should be(
      ElseOtherwiseExpr[Id](())
    )

    parseElse("otherwise") should be(
      ElseOtherwiseExpr[Id](())
    )
  }
}
