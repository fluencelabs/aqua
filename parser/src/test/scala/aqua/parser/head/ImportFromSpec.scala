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

package aqua.parser.head

import aqua.AquaSpec
import aqua.parser.expr.func.ServiceIdExpr
import aqua.parser.lexer.{LiteralToken, Token}
import aqua.parser.lift.LiftParser.given
import aqua.types.LiteralType

import cats.Id
import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ImportFromSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec.*

  "import from" should "be parsed" in {
    FromExpr.nameOrAbAs.parseAll("")

    ImportFromExpr.p.parseAll("import MyModule from \"file.aqua\"").value.mapK(spanToId) should be(
      ImportFromExpr(
        NonEmptyList.one(Right(toAb("MyModule") -> None)),
        toStr("file.aqua")
      )
    )

    ImportFromExpr.p
      .parseAll("""import MyModule, func as fn from "file.aqua"""")
      .value
      .mapK(spanToId) should be(
      ImportFromExpr(
        NonEmptyList.fromListUnsafe(
          Right(toAb("MyModule") -> None) :: Left(toName("func") -> Some(toName("fn"))) :: Nil
        ),
        toStr("file.aqua")
      )
    )
  }

}
