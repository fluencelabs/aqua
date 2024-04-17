package aqua.parser.head

import aqua.AquaSpec
import aqua.parser.expr.func.ServiceIdExpr
import aqua.parser.lexer.{LiteralToken, Token}
import aqua.parser.lift.LiftParser.given
import aqua.types.LiteralType

import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModuleSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec.*

  val myModule = ModuleExpr(
    ModuleExpr.Word[Id](Id(ModuleExpr.Word.Kind.Aqua)),
    toAb("MyModule"),
    None,
    Nil,
    Nil
  )

  val declaresAll = myModule.copy(
    declareAll = Some(Token.lift[Id, Unit](()))
  )

  def declares(symbols: List[String]) =
    myModule.copy(
      declareNames = symbols.filter(_.headOption.exists(_.isLower)).map(toName),
      declareCustom = symbols.filter(_.headOption.exists(_.isUpper)).map(toAb)
    )

  def parseModuleExpr(expr: String): ModuleExpr[Id] =
    ModuleExpr.p
      .parseAll(expr)
      .value
      .mapK(spanToId)

  "module expr" should "be parsed" in {
    parseModuleExpr("aqua MyModule") should be(myModule)
  }

  it should "be parsed with spaces in the end" in {
    (0 to 10).foreach(sp => parseModuleExpr("aqua MyModule" + " ".repeat(sp)) should be(myModule))
  }

  it should "be parsed with spaces in the beginning" in {
    (0 to 10).foreach(sp => parseModuleExpr(" ".repeat(sp) + "aqua MyModule") should be(myModule))
  }

  it should "be parsed with `declares *`" in {
    parseModuleExpr("aqua MyModule declares *") should be(declaresAll)
  }

  it should "be parsed with `declares *` with spaces in the end" in {
    (0 to 10).foreach(sp =>
      parseModuleExpr("aqua MyModule declares *" + " ".repeat(sp)) should be(declaresAll)
    )
  }

  it should "be parsed with `declares`" in {
    List("a", "myFunc", "MyService", "MyAbility", "CONST").inits.takeWhile(_.nonEmpty).foreach {
      decl =>
        parseModuleExpr(s"aqua MyModule declares " + decl.mkString(", ")) should be(
          declares(decl)
        )
    }
  }

  "module header" should "be parsed" in {
    Header.p
      .parseAll(s"""aqua MyModule declares *
                   |""".stripMargin)
      .value
      .headers
      .headOption
      .get
      .mapK(spanToId) should be(declaresAll)
  }

}
