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

class FromSpec extends AnyFlatSpec with Matchers with AquaSpec {

  import AquaSpec.*

  "from constants" should "be parsed" in {
    parseQNameAs("SOME_CONSTANT") shouldBe toQNameAs("SOME_CONSTANT", None)
    parseQNameAs("SOME_CONSTANT as SC") shouldBe toQNameAs("SOME_CONSTANT", Some("SC"))
  }

  "from expression" should "be parsed" in {
    parseQNameAs("Ability") shouldBe toQNameAs("Ability", None)
    parseQNameAs("Ability as Ab") shouldBe toQNameAs("Ability", Some("Ab"))
    parseQNameAs("function") shouldBe toQNameAs("function", None)
    parseQNameAs("function as fn") shouldBe toQNameAs("function", Some("fn"))
  }

}
