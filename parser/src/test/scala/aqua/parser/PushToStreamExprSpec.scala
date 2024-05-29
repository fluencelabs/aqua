package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.func.PushToStreamExpr
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PushToStreamExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec.{given, *}

  "assign" should "be parsed" in {
    parsePush("a <<- \"b\"") should be(
      PushToStreamExpr[Id]("a", Right(toStr("b")))
    )

    parsePush("a <<- b") should be(
      PushToStreamExpr[Id]("a", Right(toVar("b")))
    )
  }
}
