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

class FromSpec extends AnyFlatSpec with Matchers with AquaSpec {

  import AquaSpec.*

  "from constants" should "be parsed" in {
    parseNameOrAbs("SOME_CONSTANT") shouldBe Right(
      (toAb("SOME_CONSTANT"), None)
    )

    parseNameOrAbs("SOME_CONSTANT as SC") shouldBe Right(
      (toAb("SOME_CONSTANT"), Some(toAb("SC")))
    )
  }

  "from expression" should "be parsed" in {
    parseNameOrAbs("Ability") should be(Right(toAb("Ability") -> None))
    parseNameOrAbs("Ability as Ab") should be(
      Right(toAb("Ability") -> Some(toAb("Ab")))
    )
    parseNameOrAbs("function") should be(
      Left(toName("function") -> None)
    )
    parseNameOrAbs("function as fn") should be(
      Left(toName("function") -> Some(toName("fn")))
    )
  }

  "from list" should "be parsed" in {
    fromExprToId(Token.comma(FromExpr.nameOrAbAs).parseAll("Ability").value.head) should be(
      Right(toAb("Ability") -> None)
    )
    fromExprToId(Token.comma(FromExpr.nameOrAbAs).parseAll("Ability as Ab").value.head) should be(
      Right(toAb("Ability") -> Some(toAb("Ab")))
    )

    fromExprToId(FromExpr.importFrom.parseAll("Ability as Ab from").value.head) should be(
      Right(toAb("Ability") -> Some(toAb("Ab")))
    )
    fromExprToId(FromExpr.importFrom.parseAll("Ability from").value.head) should be(
      Right(toAb("Ability") -> None)
    )
  }

}
