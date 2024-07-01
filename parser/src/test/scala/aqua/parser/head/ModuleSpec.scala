/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.parser.head

import aqua.AquaSpec
import aqua.parser.expr.func.ServiceIdExpr
import aqua.parser.lexer.{LiteralToken, Token}
import aqua.parser.lift.LiftParser.given
import aqua.types.LiteralType

import cats.Id
import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModuleSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec.*

  val myModule = ModuleExpr(
    ModuleExpr.Word[Id](Id(ModuleExpr.Word.Kind.Aqua)),
    toQName("MyModule"),
    None
  )

  val declaresAll = myModule.copy(
    declares = Some(ModuleExpr.Declares.All(Token.lift[Id, Unit](())))
  )

  def declares(symbols: List[String]) =
    myModule.copy(
      declares = Some(ModuleExpr.Declares.Names(NonEmptyList.fromListUnsafe(symbols.map(toQName))))
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
