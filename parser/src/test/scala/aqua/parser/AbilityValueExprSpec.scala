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
import aqua.AquaSpec.{toNumber, toStr, toVar}
import aqua.parser.expr.ConstantExpr
import aqua.parser.expr.func.AssignmentExpr
import aqua.parser.lexer.CollectionToken.Mode.ArrayMode
import aqua.parser.lexer.*
import aqua.types.LiteralType
import cats.Id
import cats.data.{NonEmptyList, NonEmptyMap}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AbilityValueExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec.*

  private def parseAndCheckAbility(str: String) = {
    parseData(str) should be(
      NamedValueToken(
        NamedTypeToken[Id]("AbilityA"),
        NonEmptyList.of(
          NamedArg.Full(toName("v1"), toNumber(1)),
          NamedArg.Full(
            toName("f1"),
            PropertyToken(
              VarToken(toName("input")),
              NonEmptyList.one(IntoField("arrow"))
            )
          )
        )
      )
    )
  }

  "one line struct value" should "be parsed" in {
    parseAndCheckAbility("""AbilityA(v1 = 1, f1 = input.arrow)""")
  }

  "multiline line struct value" should "be parsed" in {
    parseAndCheckAbility("""AbilityA(v1 = 1, f1 = input.arrow)""".stripMargin)
  }

}
