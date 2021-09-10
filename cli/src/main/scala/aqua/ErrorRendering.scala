package aqua

import aqua.compiler.*
import aqua.files.FileModuleId
import aqua.io.AquaFileError
import aqua.parser.lift.{FileSpan, Span}
import aqua.parser.{BlockIndentError, FuncReturnError, LexerError}
import aqua.semantics.{HeaderError, RulesViolated, WrongAST}
import cats.{Eval, Show}
import cats.parse.LocationMap
import cats.parse.Parser.Expectation
import cats.parse.Parser.Expectation.*

object ErrorRendering {

  def expectationToString(expectation: Expectation, locationMap: Eval[LocationMap], currentOffset: Int): String = {
    // add column number if it is not on the current offset
    def makeMsg(msg: String, offset: Int): String = {
      if (offset == currentOffset) msg
      else {
        val focus = Span(offset, offset + 1).focus(locationMap, 0)
        focus.map(f => s"$msg on ${f.line._1}:${f.column}").getOrElse(msg)
      }
    }

    // TODO: match all expectations
    expectation match {
      case wc@WithContext(str, offset) => makeMsg(str, wc.offset)
      case InRange(offset, lower, upper) =>
        if (lower == upper)
          makeMsg(s"Expected symbol '$lower'", offset)
        else
          makeMsg(s"Expected symbols from '$lower' to '$upper'", offset)
      case OneOfStr(offset, strs) =>
        makeMsg(s"Expected one of these strings: ${strs.map(s => s"'$s'").mkString(", ")}", offset)
      case e => "Expected: " + e.toString
    }
  }

  def showForConsole(span: FileSpan, messages: List[String]): String =
    span
      .focus(3)
      .map(
        _.toConsoleStr(
          messages,
          Console.RED
        )
      )
      .getOrElse(
        "(offset is beyond the script, syntax errors) Error: " + Console.RED + messages.mkString(", ")
      ) + Console.RESET + "\n"

  implicit val showError: Show[AquaError[FileModuleId, AquaFileError, FileSpan.F]] = Show.show {
    case ParserErr(err) =>
      err match {
        case BlockIndentError(indent, message) => showForConsole(indent._1, message :: Nil)
        case FuncReturnError(point, message) => showForConsole(point._1, message :: Nil)
        case LexerError((span, e)) =>
          span
            .focus(3)
            .map { spanFocus =>
              val errorMessages = e.expected.map(exp => expectationToString(exp, span.locationMap, span.span.startIndex))
              spanFocus.toConsoleStr(
                s"Syntax error: ${errorMessages.head}" :: errorMessages.tail.map(t => "OR " + t),
                Console.RED
              )
            }
            .getOrElse(
              "(offset is beyond the script, syntax errors) " + Console.RED + e.expected.toList
                .mkString(", ")
            ) + Console.RESET + "\n"
      }
    case SourcesErr(err) =>
      Console.RED + err.showForConsole + Console.RESET
    case ResolveImportsErr(_, token, err) =>
      val span = token.unit._1
      showForConsole(span, s"Cannot resolve imports: ${err.showForConsole}" :: Nil)

    case ImportErr(token) =>
      val span = token.unit._1
      showForConsole(span, s"Cannot resolve import" :: Nil)
    case CycleError(modules) =>
      s"Cycle loops detected in imports: ${modules.map(_.file.fileName)}"
    case CompileError(err) =>
      err match {
        case RulesViolated(token, message) =>
          token.unit._1
            .focus(2)
            .map(_.toConsoleStr(message :: Nil, Console.CYAN))
            .getOrElse("(Dup error, but offset is beyond the script)") + "\n"
        case HeaderError(token, message) =>
          token.unit._1
            .focus(2)
            .map(_.toConsoleStr(message :: Nil, Console.CYAN))
            .getOrElse("(Dup error, but offset is beyond the script)") + "\n"
        case WrongAST(ast) =>
          s"Semantic error"

      }

    case OutputError(_, err) =>
      Console.RED + err.showForConsole + Console.RESET
  }
}
