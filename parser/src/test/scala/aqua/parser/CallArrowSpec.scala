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
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.{CallArrowToken, IntoArrow, Name, PropertyToken, VarToken}

import cats.data.NonEmptyList
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CallArrowSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec._

  "func calls" should "parse func()" in {
    parseExpr("func()") should be(
      CallArrowExpr[Id](Nil, CallArrowToken(toName("func"), Nil))
    )

    parseExpr("Ab.func(arg)") should be(
      CallArrowExpr[Id](
        Nil,
        PropertyToken[Id](
          VarToken[Id](toName("Ab")),
          NonEmptyList.one(
            IntoArrow(toName("func"), toVar("arg") :: Nil)
          )
        )
      )
    )

    parseExpr("func(arg.doSomething)") should be(
      CallArrowExpr[Id](
        Nil,
        CallArrowToken(Name[Id]("func"), List(toVarLambda("arg", List("doSomething"))))
      )
    )

    parseExpr("func(arg.doSomething.and.doSomethingElse)") should be(
      CallArrowExpr[Id](
        Nil,
        CallArrowToken(
          Name[Id]("func"),
          List(toVarLambda("arg", List("doSomething", "and", "doSomethingElse")))
        )
      )
    )

    parseExpr("func(arg.doSomething.and.doSomethingElse)") should be(
      CallArrowExpr[Id](
        Nil,
        CallArrowToken(
          Name[Id]("func"),
          List(toVarLambda("arg", List("doSomething", "and", "doSomethingElse")))
        )
      )
    )

    parseExpr("Ab.func(arg.doSomething.and.doSomethingElse, arg2.someFunc)") should be(
      CallArrowExpr[Id](
        Nil,
        PropertyToken[Id](
          VarToken[Id](toName("Ab")),
          NonEmptyList.one(
            IntoArrow(
              toName("func"),
              List(
                toVarLambda("arg", List("doSomething", "and", "doSomethingElse")),
                toVarLambda("arg2", List("someFunc"))
              )
            )
          )
        )
      )
    )

    parseExpr("x <- func(arg.doSomething)") should be(
      CallArrowExpr[Id](
        List(toName("x")),
        CallArrowToken(
          Name[Id]("func"),
          List(
            toVarLambda("arg", List("doSomething"))
          )
        )
      )
    )

    parseExpr("x, y, z <- func(arg.doSomething)") should be(
      CallArrowExpr[Id](
        toName("x") :: toName("y") :: toName("z") :: Nil,
        CallArrowToken(
          Name[Id]("func"),
          List(
            toVarLambda("arg", List("doSomething"))
          )
        )
      )
    )
  }
}
