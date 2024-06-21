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
import aqua.parser.expr.func.OnExpr
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OnExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec._

  "on" should "be parsed" in {
    parseOn("on peer") should be(
      OnExpr[Id](toVar("peer"), Nil)
    )

    parseOn("on peer.id") should be(
      OnExpr[Id](toVarLambda("peer", List("id")), Nil)
    )

    parseOn("on \"peer\"") should be(
      OnExpr[Id](toStr("peer"), Nil)
    )

    parseOn("on 1") should be(
      OnExpr[Id](toNumber(1), Nil)
    )

    parseOn("on \"asd\" via \"fre\" via \"fre2\"") should be(
      OnExpr[Id](toStr("asd"), List(toStr("fre"), toStr("fre2")))
    )
  }
}
