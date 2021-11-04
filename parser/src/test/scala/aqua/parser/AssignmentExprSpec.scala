package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.ConstantExpr
import aqua.parser.expr.func.AssignmentExpr
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

    parseConstant("const A = B") should be(
      ConstantExpr[Id]("A", toVar("B"), skipIfAlreadyDefined = false)
    )

    parseConstant("const A = 1") should be(
      ConstantExpr[Id]("A", toNumber(1), skipIfAlreadyDefined = false)
    )

    parseConstant("const A ?= 1") should be(
      ConstantExpr[Id]("A", toNumber(1), skipIfAlreadyDefined = true)
    )
  }
}
