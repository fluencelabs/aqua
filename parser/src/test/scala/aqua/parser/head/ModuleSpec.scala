package aqua.parser.head

import aqua.AquaSpec
import aqua.parser.expr.func.ServiceIdExpr
import aqua.parser.lexer.{LiteralToken, Token}
import aqua.parser.lift.LiftParser.Implicits.*
import aqua.types.LiteralType

import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModuleSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec.*

  "module header" should "be parsed" in {
    ModuleExpr.p.parseAll("aqua MyModule").value.mapK(spanToId) should be(
      ModuleExpr(
        toAb("MyModule"),
        None,
        Nil,
        Nil
      )
    )

    Header.p
      .parseAll(s"""aqua MyModule declares *
                   |""".stripMargin)
      .value
      .headOption
      .get
      .mapK(spanToId) should be(
      ModuleExpr(
        toAb("MyModule"),
        Some(Token.lift[Id, Unit](())),
        Nil,
        Nil
      )
    )
  }

}
