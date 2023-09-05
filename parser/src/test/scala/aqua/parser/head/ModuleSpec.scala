package aqua.parser.head

import aqua.AquaSpec
import aqua.parser.expr.func.ServiceIdExpr
import aqua.parser.lexer.{LiteralToken, Token}
import aqua.types.LiteralType
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.*

class ModuleSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec.*

  "module header" should "be parsed" in {
    ModuleExpr.p.parseAll("module MyModule").value.mapK(spanToId) should be(
      ModuleExpr(
        toAb("MyModule"),
        None,
        Nil,
        Nil
      )
    )

    HeadExpr
      .ast
      .parseAll(s"""module MyModule declares *
                   |""".stripMargin)
      .value
      .head.mapK(spanToId) should be(
      ModuleExpr(
        toAb("MyModule"),
        Some(Token.lift[Id, Unit](())),
        Nil,
        Nil
      )
    )
  }

}
