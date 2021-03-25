package aqua.parser

import aqua.Utils
import aqua.parser.expr.{OnExpr, ParExpr}
import aqua.semantics.LiteralType.{number, string}
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParExprSpec extends AnyFlatSpec with Matchers with Utils {
  import Utils._

  "on" should "be parsed" in {
    parsePar("par") should be(
      ParExpr[Id](())
    )
  }
}
