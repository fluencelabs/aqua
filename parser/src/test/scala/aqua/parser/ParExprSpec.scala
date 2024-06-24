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
import aqua.AquaSpec.*
import aqua.parser.expr.func.{CallArrowExpr, ForExpr, JoinExpr, OnExpr, ParExpr}
import aqua.parser.lexer.{CallArrowToken, Token}
import aqua.parser.lift.LiftParser.given

import cats.data.{Chain, NonEmptyList}
import cats.free.Cofree
import cats.{Eval, Id}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParExprSpec extends AnyFlatSpec with Matchers with Inside with AquaSpec {

  def insidePar(str: String)(testFun: Ast.Tree[Id] => Any) =
    inside(ParExpr.readLine.parseAll(str).map(_.map(_.mapK(spanToId)).forceAll)) {
      case Right(tree) => testFun(tree)
    }

  def par(expr: Expr[Id]): Ast.Tree[Id] =
    Cofree(
      ParExpr(Token.lift(())),
      Eval.now(
        Chain(
          Cofree(
            expr,
            Eval.now(Chain.empty)
          )
        )
      )
    )

  "par" should "be parsed" in {
    insidePar("par x <- y()")(
      _ should be(
        par(
          CallArrowExpr(
            List(toName("x")),
            CallArrowToken(toName("y"), Nil)
          )
        )
      )
    )

    insidePar("par call()")(
      _ should be(
        par(
          CallArrowExpr(
            Nil,
            CallArrowToken(toName("call"), Nil)
          )
        )
      )
    )

    insidePar("par on call() via relay:")(
      _ should be(
        par(
          OnExpr(
            CallArrowToken(toName("call"), Nil),
            toVar("relay") :: Nil
          )
        )
      )
    )

    insidePar("par join call(), x")(
      _ should be(
        par(
          JoinExpr(
            NonEmptyList.of(
              CallArrowToken(toName("call"), Nil),
              toVar("x")
            )
          )
        )
      )
    )

    insidePar("par for w <- getWorkers():")(
      _ should be(
        par(
          ForExpr(
            Right(toName("w")),
            CallArrowToken(toName("getWorkers"), Nil),
            None
          )
        )
      )
    )
  }
}
