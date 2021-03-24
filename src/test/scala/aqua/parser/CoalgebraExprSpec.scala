package aqua.parser

import aqua.Utils
import aqua.parser.expr.CoalgebraExpr
import aqua.parser.lexer.{Name, VarLambda}
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CoalgebraExprSpec extends AnyFlatSpec with Matchers with Utils {
  import Utils._

  "func calls" should "parse func()" in {
    parseExpr("func()") should be(CoalgebraExpr[Id](None, None, toName("func"), List()))
    parseExpr("Ab.func(arg)") should be(
      CoalgebraExpr[Id](None, Some(toAb("Ab")), Name[Id]("func"), List(VarLambda[Id](toName("arg"))))
    )

    parseExpr("func(arg.doSomething)") should be(
      CoalgebraExpr[Id](None, None, Name[Id]("func"), List(toVar("arg", List("doSomething"))))
    )

    parseExpr("func(arg.doSomething.and.doSomethingElse)") should be(
      CoalgebraExpr[Id](None, None, Name[Id]("func"), List(toVar("arg", List("doSomething", "and", "doSomethingElse"))))
    )

    parseExpr("Ab.func(arg.doSomething.and.doSomethingElse, arg2.someFunc)") should be(
      CoalgebraExpr[Id](
        None,
        Some(toAb("Ab")),
        Name[Id]("func"),
        List(toVar("arg", List("doSomething", "and", "doSomethingElse")), toVar("arg2", List("someFunc")))
      )
    )

    parseExpr("x <- func(arg.doSomething)") should be(
      CoalgebraExpr[Id](
        Some(toName("x")),
        None,
        Name[Id]("func"),
        List(
          toVar("arg", List("doSomething"))
        )
      )
    )
  }
}
