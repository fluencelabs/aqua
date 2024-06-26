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
import aqua.parser.expr.func.IfExpr
import aqua.parser.lexer.InfixToken.Op.{Add, Sub}
import aqua.parser.lexer.{
  CallArrowToken,
  CollectionToken,
  InfixToken,
  IntoArrow,
  PropertyToken,
  ValueToken,
  VarToken
}
import aqua.parser.lexer.CollectionToken.Mode.OptionMode

import cats.Id
import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IfExprSpec extends AnyFlatSpec with Matchers with AquaSpec {

  import AquaSpec.*

  "if" should "be parsed" in {
    parseIf("if a") should be(
      IfExpr[Id](toVarLambda("a", Nil))
    )

    parseIf("if a == b") should be(
      IfExpr[Id](equ(toVarLambda("a", Nil), toVar("b")))
    )

    parseIf("if 1 != false") should be(
      IfExpr[Id](neq(toNumber(1), toBool(false)))
    )

    parseIf("if a[1] != \"ds\"") should be(
      IfExpr[Id](neq(toVarIndex("a", 1), toStr("ds")))
    )

    parseIf("if a[1] == 43") should be(
      IfExpr[Id](equ(toVarIndex("a", 1), toNumber(43)))
    )

    parseIf("if a!5 == b[3]") should be(
      IfExpr[Id](equ(toVarIndex("a", 5), toVarIndex("b", 3)))
    )

    parseIf("if Op.identity(\"str\") == \"a\"") should be(
      IfExpr[Id](
        equ(
          PropertyToken[Id](
            VarToken[Id](toName("Op")),
            NonEmptyList.one(
              IntoArrow(toName("identity"), toStr("str") :: Nil)
            )
          ),
          toStr("a")
        )
      )
    )

    parseIf("if Op.identity(\"str\") != Op.identity(\"str\")") should be {
      val operand = PropertyToken[Id](
        VarToken[Id](toName("Op")),
        NonEmptyList.one(
          IntoArrow(toName("identity"), toStr("str") :: Nil)
        )
      )
      IfExpr[Id](
        neq(
          operand,
          operand
        )
      )
    }

    parseIf("if 2 - 3 != Op.identity(4) + 5") should be(
      IfExpr[Id](
        neq(
          sub(toNumber(2), toNumber(3)),
          add(
            PropertyToken[Id](
              VarToken[Id](toName("Op")),
              NonEmptyList.one(
                IntoArrow(toName("identity"), toNumber(4) :: Nil)
              )
            ),
            toNumber(5)
          )
        )
      )
    )

    parseIf("if funcCall(3) == funcCall2(4)") should be(
      IfExpr[Id](
        equ(
          CallArrowToken[Id](toName("funcCall"), toNumber(3) :: Nil),
          CallArrowToken[Id](toName("funcCall2"), toNumber(4) :: Nil)
        )
      )
    )

    parseIf("if ?[\"a\"] == ?[\"a\"]") should be(
      IfExpr[Id](
        equ(
          CollectionToken[Id](OptionMode, toStr("a") :: Nil),
          CollectionToken[Id](OptionMode, toStr("a") :: Nil)
        )
      )
    )
  }
}
