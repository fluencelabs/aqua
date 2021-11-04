package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.func.IfExpr
import aqua.parser.lexer.EqOp
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IfExprSpec extends AnyFlatSpec with Matchers with AquaSpec {

  import AquaSpec._

  "if" should "be parsed" in {
    parseIf("if a") should be(
      IfExpr[Id](toVarLambda("a", Nil), EqOp[Id](true), toBool(true))
    )

    parseIf("if a == b") should be(
      IfExpr[Id](toVarLambda("a", Nil), EqOp[Id](true), toVar("b"))
    )

    parseIf("if 1 != false") should be(
      IfExpr[Id](toNumber(1), EqOp[Id](false), toBool(false))
    )
  }
}
