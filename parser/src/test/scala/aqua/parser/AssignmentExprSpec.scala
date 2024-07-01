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
import aqua.parser.expr.ConstantExpr
import aqua.parser.expr.func.AssignmentExpr
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AssignmentExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec.{given, *}

  "assign" should "be parsed" in {
    parseAssign("a = \"b\"") should be(
      AssignmentExpr[Id]("a", toStr("b"))
    )

    parseAssign("a = b") should be(
      AssignmentExpr[Id]("a", toVar("b"))
    )

    parseConstant("const A = B") should be(
      ConstantExpr[Id]("A", toVar("B"), skipIfAlreadyDefined = false)
    )

    parseConstant("const A = 1") should be(
      ConstantExpr[Id]("A", toNumber(1), skipIfAlreadyDefined = false)
    )

    parseConstant("const A ?= 1") should be(
      ConstantExpr[Id]("A", toNumber(1), skipIfAlreadyDefined = true)
    )
  }
}
