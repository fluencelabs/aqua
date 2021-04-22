package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.{AssignmentExpr, ConstantExpr}
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AssignmentExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec._

  "assign" should "be parsed" in {
    parseAssign("a = \"b\"") should be(
      AssignmentExpr[Id]("a", toStr("b"))
    )

    parseAssign("a = b") should be(
      AssignmentExpr[Id]("a", toVar("b"))
    )

    parseConstant("const a = b") should be(
      ConstantExpr[Id]("a", toVar("b"), skipIfAlreadyDefined = false)
    )

    parseConstant("const a = 1") should be(
      ConstantExpr[Id]("a", toNumber(1), skipIfAlreadyDefined = false)
    )

    parseConstant("const a ?= 1") should be(
      ConstantExpr[Id]("a", toNumber(1), skipIfAlreadyDefined = true)
    )
  }
}
