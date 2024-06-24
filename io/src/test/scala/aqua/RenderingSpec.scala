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

package aqua.files

import aqua.Rendering.given
import aqua.compiler.AquaError
import aqua.files.FileModuleId
import aqua.io.AquaFileError
import aqua.parser.LexerError
import aqua.parser.lift.{FileSpan, Span}

import cats.Eval
import cats.data.NonEmptyList
import cats.parse.Parser.Expectation.InRange
import cats.parse.{LocationMap, Parser}
import cats.syntax.show.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Inside, Inspectors}

class RenderingSpec extends AnyFlatSpec with Matchers with Inside with Inspectors {

  it should "render end of a string properly" in {
    val script =
      """func arr(strs: []string) -> []string
        |  <- strs""".stripMargin

    val error = Parser.Error(8, NonEmptyList.one(InRange(36, ':', ':')))
    val fileSpan = FileSpan("file", Eval.now(LocationMap(script)), Span(8, 9))

    val result: AquaError[FileModuleId, AquaFileError, FileSpan.F] =
      AquaError.ParserError(LexerError((fileSpan, error)))

    result.show should include("Syntax error: file:1:37")
  }
}
