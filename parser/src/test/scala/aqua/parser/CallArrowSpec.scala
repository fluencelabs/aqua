package aqua.parser

import aqua.AquaSpec
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.{CallArrowToken, IntoArrow, Name, PropertyToken, VarToken}

import cats.data.NonEmptyList
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CallArrowSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec._

  "func calls" should "parse func()" in {
    parseExpr("func()") should be(
      CallArrowExpr[Id](Nil, CallArrowToken(toName("func"), Nil))
    )

    parseExpr("Ab.func(arg)") should be(
      CallArrowExpr[Id](
        Nil,
        PropertyToken[Id](
          VarToken[Id](toName("Ab")),
          NonEmptyList.one(
            IntoArrow(toName("func"), toVar("arg") :: Nil)
          )
        )
      )
    )

    parseExpr("func(arg.doSomething)") should be(
      CallArrowExpr[Id](
        Nil,
        CallArrowToken(Name[Id]("func"), List(toVarLambda("arg", List("doSomething"))))
      )
    )

    parseExpr("func(arg.doSomething.and.doSomethingElse)") should be(
      CallArrowExpr[Id](
        Nil,
        CallArrowToken(
          Name[Id]("func"),
          List(toVarLambda("arg", List("doSomething", "and", "doSomethingElse")))
        )
      )
    )

    parseExpr("func(arg.doSomething.and.doSomethingElse)") should be(
      CallArrowExpr[Id](
        Nil,
        CallArrowToken(
          Name[Id]("func"),
          List(toVarLambda("arg", List("doSomething", "and", "doSomethingElse")))
        )
      )
    )

    parseExpr("Ab.func(arg.doSomething.and.doSomethingElse, arg2.someFunc)") should be(
      CallArrowExpr[Id](
        Nil,
        PropertyToken[Id](
          VarToken[Id](toName("Ab")),
          NonEmptyList.one(
            IntoArrow(
              toName("func"),
              List(
                toVarLambda("arg", List("doSomething", "and", "doSomethingElse")),
                toVarLambda("arg2", List("someFunc"))
              )
            )
          )
        )
      )
    )

    parseExpr("x <- func(arg.doSomething)") should be(
      CallArrowExpr[Id](
        List(toName("x")),
        CallArrowToken(
          Name[Id]("func"),
          List(
            toVarLambda("arg", List("doSomething"))
          )
        )
      )
    )

    parseExpr("x, y, z <- func(arg.doSomething)") should be(
      CallArrowExpr[Id](
        toName("x") :: toName("y") :: toName("z") :: Nil,
        CallArrowToken(
          Name[Id]("func"),
          List(
            toVarLambda("arg", List("doSomething"))
          )
        )
      )
    )
  }
}
