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
import aqua.parser.expr.func.ForExpr

import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ForExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec.{given, *}

  def forTestSuite(
    modeStr: String,
    mode: Option[ForExpr.Mode]
  ): Unit = {
    parseFor(s"for some <- 1$modeStr") should be(
      ForExpr[Id](Right("some"), toNumber(1), mode)
    )

    parseFor(s"for some <- false$modeStr") should be(
      ForExpr[Id](Right("some"), toBool(false), mode)
    )

    parseFor(s"for some <- \"a\"$modeStr") should be(
      ForExpr[Id](Right("some"), toStr("a"), mode)
    )

    parseFor(s"for i <- []$modeStr") should be(
      ForExpr[Id](Right("i"), toArr(Nil), mode)
    )

    parseFor(s"for i <- [1, 2, 3]$modeStr") should be(
      ForExpr[Id](Right("i"), toArr(List(toNumber(1), toNumber(2), toNumber(3))), mode)
    )

    parseFor(s"for i <- stream$modeStr") should be(
      ForExpr[Id](Right("i"), toVar("stream"), mode)
    )
  }

  "for expression" should "be parsed" in {
    forTestSuite("", None)
  }

  "for par expression" should "be parsed" in {
    forTestSuite(" par", Some(ForExpr.Mode.ParMode))
  }

  "for try expression" should "be parsed" in {
    forTestSuite(" try", Some(ForExpr.Mode.TryMode))
  }

  "for rec expression" should "be parsed" in {
    forTestSuite(" rec", Some(ForExpr.Mode.RecMode))
  }
}
