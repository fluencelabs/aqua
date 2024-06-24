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

package aqua.parser

import aqua.AquaSpec
import aqua.parser.lexer.{IntoArrow, PropertyOp, PropertyToken, VarToken}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import cats.Id
import cats.data.NonEmptyList

class IntoArrowSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec.*

  "into arrow" should "be parsed" in {
    val arrowStr = ".arrow(\"\")"

    val result = parseIntoArrow(arrowStr)
    result should be(IntoArrow[Id](toName("arrow"), toStr("") :: Nil))
  }

  "into arrow without arguments" should "be parsed" in {
    val arrowStr = ".arrow()"

    val result = parseIntoArrow(arrowStr)
    result should be(IntoArrow[Id](toName("arrow"), Nil))
  }

  "into arrow with value" should "be parsed" in {
    val arrowStr = "input.arrow(\"\")"

    val result = parseVar(arrowStr)
    val expected = PropertyToken[Id](
      VarToken[Id](toName("input")),
      NonEmptyList.one(IntoArrow[Id](toName("arrow"), toStr("") :: Nil))
    )

    result should be(expected)
  }
}
