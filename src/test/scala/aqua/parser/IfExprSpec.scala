package aqua.parser

import aqua.Utils
import aqua.parser.expr.{ForExpr, IfExpr}
import aqua.parser.lexer.EqOp
import aqua.semantics.LiteralType.{bool, number, string}
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IfExprSpec extends AnyFlatSpec with Matchers with Utils {

  import Utils._

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
