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

import aqua.parser.lexer.Token._
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TokenSpec extends AnyFlatSpec with Matchers with EitherValues {

  "\\n token" should "be parsed" in {
    ` \n`.parseAll("\n").isRight should be(true)
    ` \n`.parseAll(" \n").isRight should be(true)
    ` \n`.parseAll("         \n").isRight should be(true)
    ` \n`.parseAll("               \n").isRight should be(true)
    ` \n`.parseAll("--comment\n").isRight should be(true)
    ` \n`.parseAll(" --comment\n").isRight should be(true)
    ` \n`.parseAll("    --comment\n").isRight should be(true)
    ` \n`.parseAll("    --comment with many words\n").isRight should be(true)
    ` \n`.parseAll("    --comment with many words      \n").isRight should be(true)
    ` \n`.parse("    --comment with many words      \n").value should be(("", ()))
    ` \n`.parse("    --comment with many words      \n     ").value should be(("     ", ()))
  }

  "\\n* token" should "match the same strings" in {
    ` \n+`.parseAll("\n").isRight should be(true)
    ` \n+`.parseAll(" \n").isRight should be(true)
    ` \n+`.parseAll("         \n").isRight should be(true)
    ` \n+`.parseAll("               \n").isRight should be(true)
    ` \n+`.parseAll("--comment\n").isRight should be(true)
    ` \n+`.parseAll(" --comment\n").isRight should be(true)
    ` \n+`.parseAll("    --comment\n").isRight should be(true)
    ` \n+`.parseAll("    --comment with many words\n").isRight should be(true)
    ` \n+`.parseAll("    --comment with many words      \n").isRight should be(true)
    ` \n+`.parse("    --comment with many words      \n").value should be(("", ()))
    ` \n+`.parse("    --comment with many words      \n     ").value should be(("     ", ()))
  }

  "\\n* token" should "match multi-line comments" in {
    ` \n+`.parseAll("""  -- comment line 1
                      |-- line 2
                      |
                      |              -- line 3
                      |              -- line 4
                      |""".stripMargin).value should be(())
  }

  "name token" should "parse" in {
    `name`.parseAll("some_name").value should be("some_name")
  }

  "NAME token" should "parse" in {
    NAME.parseAll("SOME_NAME").value should be("SOME_NAME")
  }

}
