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

import cats.Id
import cats.data.NonEmptyList
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VarLambdaSpec extends AnyFlatSpec with Matchers with EitherValues {

  import aqua.AquaSpec._

  "var lambda" should "parse" in {
    val opsP = (s: String) => ValueToken.value.parseAll(s).value.mapK(spanToId)

    opsP("some_val") should be(
      VarToken[Id](Name[Id]("some_val"))
    )

    opsP("SOME_CONST") should be(
      VarToken[Id](Name[Id]("SOME_CONST"))
    )

    opsP("SomeClass.some_val") should be(
      PropertyToken[Id](
        VarToken[Id](Name[Id]("SomeClass")),
        NonEmptyList.one(IntoField[Id]("some_val"))
      )
    )

    opsP("SomeClass.Some_Other_Class") should be(
      PropertyToken[Id](
        VarToken[Id](Name[Id]("SomeClass")),
        NonEmptyList.one(IntoField[Id]("Some_Other_Class"))
      )
    )

    opsP("SomeClass.SOME_CONST") should be(
      PropertyToken[Id](
        VarToken[Id](Name[Id]("SomeClass")),
        NonEmptyList.one(IntoField[Id]("SOME_CONST"))
      )
    )

    opsP("SomeClass.call()") should be(
      PropertyToken[Id](
        VarToken[Id](Name[Id]("SomeClass")),
        NonEmptyList.one(IntoArrow[Id](Name[Id]("call"), Nil))
      )
    )
  }

}
