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
import aqua.parser.expr.func.ParSeqExpr
import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParSeqExprSpec extends AnyFlatSpec with Matchers with AquaSpec {

  import AquaSpec.*

  "parseq" should "be parsed" in {
    parseParSeq("parseq s <- strings on \"peerId\"") should be(
      ParSeqExpr[Id](Right(toName("s")), toVar("strings"), toStr("peerId"), Nil)
    )

    parseParSeq("parseq s <- strings on \"peerId\" via \"relay\"") should be(
      ParSeqExpr[Id](Right(toName("s")), toVar("strings"), toStr("peerId"), toStr("relay") :: Nil)
    )

    parseParSeq("parseq s <- strings on \"peerId\" via \"relay\" via \"relay2\"") should be(
      ParSeqExpr[Id](
        Right(toName("s")),
        toVar("strings"),
        toStr("peerId"),
        toStr("relay") :: toStr("relay2") :: Nil
      )
    )

    parseParSeq("parseq s <- strings on peerId via relay") should be(
      ParSeqExpr[Id](Right(toName("s")), toVar("strings"), toVar("peerId"), toVar("relay") :: Nil)
    )

    parseParSeq("parseq s, v <- strings on peerId via relay") should be(
      ParSeqExpr[Id](Left((toName("s"), toName("v"))), toVar("strings"), toVar("peerId"), toVar("relay") :: Nil)
    )

  }
}
