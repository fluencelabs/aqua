package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.func.OnExpr
import aqua.parser.expr.func.ElseOtherwiseExpr
import aqua.parser.lexer.Token
import aqua.types.LiteralType.{number, string}
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ElseOtherwiseExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec._

  "else" should "be parsed" in {
    parseElse("else") should be(
      ElseOtherwiseExpr[Id](ElseOtherwiseExpr.Kind.Else, Token.lift(()))
    )

    parseElse("otherwise") should be(
      ElseOtherwiseExpr[Id](ElseOtherwiseExpr.Kind.Otherwise, Token.lift(()))
    )
  }
}
