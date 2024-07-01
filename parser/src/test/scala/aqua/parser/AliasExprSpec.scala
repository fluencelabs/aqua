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
import aqua.types.ScalarType.u32
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AliasExprSpec extends AnyFlatSpec with Matchers with AquaSpec {

  import AquaSpec.{given, *}

  "alias" should "be parsed properly" in {
    parseAlias("alias SomeAlias : u32") should be(
      AliasExpr[Id]("SomeAlias", u32)
    )

    parseAlias("alias SomeAlias : CustomType") should be(
      AliasExpr[Id]("SomeAlias", "CustomType")
    )
  }

  "alias" should "be parsed without spaces" in {
    parseAlias("alias SomeAlias:CustomType") should be(
      AliasExpr[Id]("SomeAlias", "CustomType")
    )
  }
}
