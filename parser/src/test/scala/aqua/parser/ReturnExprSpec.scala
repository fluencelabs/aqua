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
import aqua.parser.expr.func.ReturnExpr
import cats.Id
import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReturnExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec._

  "return" should "be parsed" in {
    parseReturn("<- true") should be(
      ReturnExpr[Id](NonEmptyList.one(toBool(true)))
    )
  }
}
