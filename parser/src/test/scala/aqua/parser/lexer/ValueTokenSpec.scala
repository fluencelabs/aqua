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

package aqua.parser.lexer

import aqua.parser.lift.LiftParser.given
import aqua.types.LiteralType

import cats.Id
import cats.data.NonEmptyList
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ValueTokenSpec extends AnyFlatSpec with Matchers with EitherValues {

  import aqua.AquaSpec._

  "var getter" should "parse" in {
    ValueToken.`value`.parseAll("varname").value.mapK(spanToId) should be(
      VarToken(Name[Id]("varname"))
    )

    ValueToken.`value`.parseAll("varname.field").value.mapK(spanToId) should be(
      PropertyToken[Id](
        VarToken(Name[Id]("varname")),
        NonEmptyList.one(IntoField[Id]("field"))
      )
    )

    ValueToken.`value`.parseAll("varname.field.sub").value.mapK(spanToId) should be(
      PropertyToken[Id](
        VarToken(Name[Id]("varname")),
        NonEmptyList.of(IntoField[Id]("field"), IntoField[Id]("sub"))
      )
    )
  }

  "literals" should "parse" in {
    ValueToken.`value`.parseAll("true").value.mapK(spanToId) should be(
      LiteralToken[Id]("true", LiteralType.bool)
    )
    ValueToken.`value`.parseAll("false").value.mapK(spanToId) should be(
      LiteralToken[Id]("false", LiteralType.bool)
    )

    ValueToken.`value`.parseAll("-1").value.mapK(spanToId) should be(
      LiteralToken[Id]("-1", LiteralType.signed)
    )
    ValueToken.`value`.parseAll("-1111").value.mapK(spanToId) should be(
      LiteralToken[Id]("-1111", LiteralType.signed)
    )

    ValueToken.`value`.parseAll("1").value.mapK(spanToId) should be(
      LiteralToken[Id]("1", LiteralType.unsigned)
    )
    ValueToken.`value`.parseAll("1111").value.mapK(spanToId) should be(
      LiteralToken[Id]("1111", LiteralType.unsigned)
    )

    ValueToken.`value`.parseAll("-1543").value.mapK(spanToId) should be(
      LiteralToken[Id]("-1543", LiteralType.signed)
    )

    ValueToken.`value`.parseAll("1.0").value.mapK(spanToId) should be(
      LiteralToken[Id]("1.0", LiteralType.float)
    )
    ValueToken.`value`.parseAll("1.23").value.mapK(spanToId) should be(
      LiteralToken[Id]("1.23", LiteralType.float)
    )
    ValueToken.`value`.parseAll("-1.23").value.mapK(spanToId) should be(
      LiteralToken[Id]("-1.23", LiteralType.float)
    )

    ValueToken.`value`.parseAll("\"some crazy string\"").value.mapK(spanToId) should be(
      LiteralToken[Id]("\"some crazy string\"", LiteralType.string)
    )
    // This does not work :(
//    Value.`value`.parseAll("\"some crazy string with escaped \\\" quote\"").value.mapK(spanToId) should be(
//      Literal("\"some crazy string with escaped \\\" quote\"", BasicType.string)
//    )
    val res = ValueToken.`value`.parse("\"just string\"     ").value
    (res._1, res._2.mapK(spanToId)) should be(
      ("     ", LiteralToken[Id]("\"just string\"", LiteralType.string))
    )
  }

}
