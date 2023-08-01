package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.func.IfExpr
import aqua.parser.lexer.InfixToken.Op.{Add, Sub}
import aqua.parser.lexer.{CallArrowToken, CollectionToken, InfixToken}
import aqua.parser.lexer.CollectionToken.Mode.OptionMode
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IfExprSpec extends AnyFlatSpec with Matchers with AquaSpec {

  import AquaSpec.*

  "if" should "be parsed" in {
    parseIf("if a") should be(
      IfExpr[Id](toVarLambda("a", Nil))
    )

    parseIf("if a == b") should be(
      IfExpr[Id](equ(toVarLambda("a", Nil), toVar("b")))
    )

    parseIf("if 1 != false") should be(
      IfExpr[Id](neq(toNumber(1), toBool(false)))
    )

    parseIf("if a[1] != \"ds\"") should be(
      IfExpr[Id](neq(toVarIndex("a", 1), toStr("ds")))
    )

    parseIf("if a[1] == 43") should be(
      IfExpr[Id](equ(toVarIndex("a", 1), toNumber(43)))
    )

    parseIf("if a!5 == b[3]") should be(
      IfExpr[Id](equ(toVarIndex("a", 5), toVarIndex("b", 3)))
    )

    parseIf("if Op.identity(\"str\") == \"a\"") should be(
      IfExpr[Id](
        equ(
          CallArrowToken[Id](Some(toNamedType("Op")), toName("identity"), toStr("str") :: Nil),
          toStr("a")
        )
      )
    )

    parseIf("if Op.identity(\"str\") != Op.identity(\"str\")") should be(
      IfExpr[Id](
        neq(
          CallArrowToken[Id](Some(toNamedType("Op")), toName("identity"), toStr("str") :: Nil),
          CallArrowToken[Id](Some(toNamedType("Op")), toName("identity"), toStr("str") :: Nil)
        )
      )
    )

    parseIf("if 2 - 3 != Op.identity(4) + 5") should be(
      IfExpr[Id](
        neq(
          sub(toNumber(2), toNumber(3)),
          add(
            CallArrowToken[Id](Some(toNamedType("Op")), toName("identity"), toNumber(4) :: Nil),
            toNumber(5)
          )
        )
      )
    )

    parseIf("if funcCall(3) == funcCall2(4)") should be(
      IfExpr[Id](
        equ(
          CallArrowToken[Id](None, toName("funcCall"), toNumber(3) :: Nil),
          CallArrowToken[Id](None, toName("funcCall2"), toNumber(4) :: Nil)
        )
      )
    )

    parseIf("if ?[\"a\"] == ?[\"a\"]") should be(
      IfExpr[Id](
        equ(
          CollectionToken[Id](OptionMode, toStr("a") :: Nil),
          CollectionToken[Id](OptionMode, toStr("a") :: Nil)
        )
      )
    )
  }
}
