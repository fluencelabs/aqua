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
import aqua.parser.expr.AliasExpr
import aqua.parser.lexer.CollectionToken
import aqua.types.ScalarType.u32
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CollectionExprSpec extends AnyFlatSpec with Matchers with AquaSpec {

  import AquaSpec.*

  "collection" should "be parsed properly" in {
    parseCollection(
      """[
        |  "1",
        |"2"
        |]""".stripMargin) should be(
      CollectionToken[Id](CollectionToken.Mode.ArrayMode, List(toStr("1"), toStr("2")))
    )
  }
}
