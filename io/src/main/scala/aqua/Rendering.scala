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

package aqua

import aqua.compiler.*
import aqua.compiler.AquaError.{ParserError as AquaParserError, *}
import aqua.files.FileModuleId
import aqua.io.AquaFileError
import aqua.parser.lift.{FileSpan, Span}
import aqua.parser.{ArrowReturnError, BlockIndentError, LexerError, ParserError}
import aqua.semantics.{HeaderError, RulesViolated, SemanticWarning, WrongAST}

import cats.Show

object Rendering {

  def showForConsole(messageType: String, span: FileSpan, messages: List[String]): String =
    span
      .focus(3)
      .map(
        _.toConsoleStr(
          messageType,
          messages,
          Console.RED
        )
      )
      .getOrElse(
        "(offset is beyond the script, syntax errors) Error: " + Console.RED + messages.mkString(
          ", "
        )
      ) + Console.RESET + "\n"

  given Show[AquaWarning[FileSpan.F]] = Show.show { case AquaWarning.CompileWarning(warning) =>
    warning match {
      case SemanticWarning(token, hints) =>
        token.unit._1
          .focus(0)
          .map(_.toConsoleStr("Warning", hints, Console.YELLOW))
          .getOrElse("(Dup warning, but offset is beyond the script)")
    }
  }

  given Show[AquaError[FileModuleId, AquaFileError, FileSpan.F]] = Show.show {
    case AquaParserError(err) =>
      err match {
        case BlockIndentError(indent, message) =>
          showForConsole("Syntax error", indent._1, message :: Nil)
        case ArrowReturnError(point, message) =>
          showForConsole("Syntax error", point._1, message :: Nil)
        case LexerError((span, e)) =>
          e.expected.toList
            .groupBy(_.offset)
            .map { case (offset, exps) =>
              val localSpan = Span(offset, offset + 1)
              val msg = FileSpan(span.name, span.locationMap, localSpan)
                .focus(0)
                .map { spanFocus =>
                  val errorMessages = exps.flatMap(exp => ParserError.expectationToString(exp))
                  spanFocus.toConsoleStr(
                    "Syntax error",
                    s"${errorMessages.head}" :: errorMessages.tail.map(t => "OR " + t),
                    Console.RED
                  )
                }
                .getOrElse(
                  "(offset is beyond the script, syntax errors) " + Console.RED + e.expected.toList
                    .mkString(", ")
                ) + Console.RESET
              (offset, msg)
            }
            .toList
            .sortBy(_._1)
            .map(_._2)
            .reverse
            .mkString("\n")
      }
    case SourcesError(err) =>
      Console.RED + err.showForConsole + Console.RESET
    case AirValidationError(errors) =>
      Console.RED + errors.toChain.toList.mkString("\n") + Console.RESET
    case ResolveImportsError(_, token, err) =>
      val span = token.unit._1
      showForConsole("Cannot resolve imports", span, err.showForConsole :: Nil)

    case ImportError(token) =>
      val span = token.unit._1
      showForConsole("Cannot resolve import", span, "Cannot resolve import" :: Nil)
    case CycleError(modules) =>
      val cycleFileNames = (
        modules.toChain.toList :+ modules.head
      ).map(_.file.fileName)
      val message = cycleFileNames
        .sliding(2)
        .collect { case prev :: next :: Nil =>
          s"$prev imports $next"
        }
        .mkString(", ")
      s"Cycle loops detected in imports: $message"
    case CompileError(err) =>
      err match {
        case RulesViolated(token, messages) =>
          token.unit._1
            .focus(0)
            .map(_.toConsoleStr("Error", messages, Console.CYAN))
            .getOrElse("(Dup error, but offset is beyond the script)")
        case HeaderError(token, message) =>
          token.unit._1
            .focus(0)
            .map(_.toConsoleStr("Header error", message :: Nil, Console.CYAN))
            .getOrElse("(Dup error, but offset is beyond the script)")
        case WrongAST(ast) =>
          "Semantic error: wrong AST"

      }

    case OutputError(_, err) =>
      Console.RED + err.showForConsole + Console.RESET
  }
}
