package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.CallArrowExpr
import aqua.parser.lexer.{Name, VarLambda}
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CallArrowSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec._

  "func calls" should "parse func()" in {
    parseExpr("func()") should be(CallArrowExpr[Id](None, None, toName("func"), List(), None))
    parseExpr("Ab.func(arg)") should be(
      CallArrowExpr[Id](
        None,
        Some(toAb("Ab")),
        Name[Id]("func"),
        List(VarLambda[Id](toName("arg"))),
        None
      )
    )

    parseExpr("func(arg.doSomething)") should be(
      CallArrowExpr[Id](
        None,
        None,
        Name[Id]("func"),
        List(toVarLambda("arg", List("doSomething"))),
        None
      )
    )

    parseExpr("func(arg.doSomething.and.doSomethingElse)") should be(
      CallArrowExpr[Id](
        None,
        None,
        Name[Id]("func"),
        List(toVarLambda("arg", List("doSomething", "and", "doSomethingElse"))),
        None
      )
    )

    parseExpr("par func(arg.doSomething.and.doSomethingElse)") should be(
      CallArrowExpr[Id](
        None,
        None,
        Name[Id]("func"),
        List(toVarLambda("arg", List("doSomething", "and", "doSomethingElse"))),
        Some(())
      )
    )

    parseExpr("Ab.func(arg.doSomething.and.doSomethingElse, arg2.someFunc)") should be(
      CallArrowExpr[Id](
        None,
        Some(toAb("Ab")),
        Name[Id]("func"),
        List(
          toVarLambda("arg", List("doSomething", "and", "doSomethingElse")),
          toVarLambda("arg2", List("someFunc"))
        ),
        None
      )
    )

    parseExpr("x <- func(arg.doSomething)") should be(
      CallArrowExpr[Id](
        Some(toName("x")),
        None,
        Name[Id]("func"),
        List(
          toVarLambda("arg", List("doSomething"))
        ),
        None
      )
    )
  }
}
