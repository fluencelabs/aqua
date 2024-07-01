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
import aqua.parser.expr.func.ElseOtherwiseExpr
import aqua.parser.lexer.Token
import aqua.types.LiteralType.{number, string}
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ElseOtherwiseExprSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec._

  "else" should "be parsed" in {
    parseElseOtherwise("else") should be(
      ElseOtherwiseExpr[Id](ElseOtherwiseExpr.Kind.Else, Token.lift(()))
    )
  }

  "otherwise" should "be parsed" in {
    parseElseOtherwise("otherwise") should be(
      ElseOtherwiseExpr[Id](ElseOtherwiseExpr.Kind.Otherwise, Token.lift(()))
    )
  }
}
