package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.func.IfExpr
import aqua.parser.lexer.InfixToken.Op.{Add, Sub}
import aqua.parser.lexer.{CallArrowToken, EqOp, InfixToken}
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

    parseIf("if a[1] != \"ds\"") should be(
      IfExpr[Id](toVarIndex("a", 1), EqOp[Id](false), toStr("ds"))
    )

    parseIf("if a[1] == 43") should be(
      IfExpr[Id](toVarIndex("a", 1), EqOp[Id](true), toNumber(43))
    )

    parseIf("if a!5 == b[3]") should be(
      IfExpr[Id](toVarIndex("a", 5), EqOp[Id](true), toVarIndex("b", 3))
    )

    parseIf("if Op.identity(\"str\") == \"a\"") should be(
      IfExpr[Id](
        CallArrowToken[Id](Some(toAb("Op")), toName("identity"), toStr("str") :: Nil),
        EqOp[Id](true),
        toStr("a")
      )
    )

    parseIf("if Op.identity(\"str\") != Op.identity(\"str\")") should be(
      IfExpr[Id](
        CallArrowToken[Id](Some(toAb("Op")), toName("identity"), toStr("str") :: Nil),
        EqOp[Id](false),
        CallArrowToken[Id](Some(toAb("Op")), toName("identity"), toStr("str") :: Nil)
      )
    )

    parseIf("if 2 - 3 != Op.identity(4) + 5") should be(
      IfExpr[Id](
        InfixToken[Id](toNumber(2), toNumber(3), Sub),
        EqOp[Id](false),
        InfixToken[Id](
          CallArrowToken[Id](Some(toAb("Op")), toName("identity"), toNumber(4) :: Nil),
          toNumber(5),
          Add
        )
      )
    )

    parseIf("if funcCall(3) == funcCall2(4)") should be(
      IfExpr[Id](
        CallArrowToken[Id](None, toName("funcCall"), toNumber(3) :: Nil),
        EqOp[Id](true),
        CallArrowToken[Id](None, toName("funcCall2"), toNumber(4) :: Nil)
      )
    )
  }
}
