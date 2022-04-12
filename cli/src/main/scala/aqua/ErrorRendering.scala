package aqua

import aqua.compiler.*
import aqua.files.FileModuleId
import aqua.io.AquaFileError
import aqua.parser.lift.{FileSpan, Span}
import aqua.parser.{ArrowReturnError, BlockIndentError, LexerError}
import aqua.semantics.{HeaderError, RulesViolated, WrongAST}
import cats.parse.LocationMap
import cats.parse.Parser.Expectation
import cats.parse.Parser.Expectation.*
import cats.{Eval, Show}

object ErrorRendering {

  def betterSymbol(symbol: Char): String = {
    symbol match {
      case ' ' => "whitespace"
      case '\t' => "tabulation"
      case c => c.toString
    }
  }

  def expectationToString(expectation: Expectation, acc: List[String] = Nil): List[String] = {
    // TODO: match all expectations
    expectation match {
      // get the deepest context
      case WithContext(str, exp: WithContext) => expectationToString(exp, List(str))
      case WithContext(str, exp) => s"$str (${expectationToString(exp)})" +: acc
      case FailWith(_, message) => message +: acc
      case InRange(offset, lower, upper) =>
        if (lower == upper)
          s"Expected symbol '${betterSymbol(lower)}'" +: acc
        else
          s"Expected symbols from '${betterSymbol(lower)}' to '${betterSymbol(upper)}'" +: acc
      case OneOfStr(offset, strs) =>
        s"Expected one of these strings: ${strs.map(s => s"'$s'").mkString(", ")}" +: acc
      case e => ("Expected: " + e.toString) +: acc
    }
  }

  def showForConsole(errorType: String, span: FileSpan, messages: List[String]): String =
    span
      .focus(3)
      .map(
        _.toConsoleStr(
          errorType,
          messages,
          Console.RED
        )
      )
      .getOrElse(
        "(offset is beyond the script, syntax errors) Error: " + Console.RED + messages.mkString(
          ", "
        )
      ) + Console.RESET + "\n"

  implicit val showError: Show[AquaError[FileModuleId, AquaFileError, FileSpan.F]] = Show.show {
    case ParserErr(err) =>
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
                  val errorMessages = exps.flatMap(exp => expectationToString(exp))
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
    case SourcesErr(err) =>
      Console.RED + err.showForConsole + Console.RESET
    case ResolveImportsErr(_, token, err) =>
      val span = token.unit._1
      showForConsole("Cannot resolve imports", span, s"${err.showForConsole}" :: Nil)

    case ImportErr(token) =>
      val span = token.unit._1
      showForConsole("Cannot resolve import", span, "Cannot resolve import" :: Nil)
    case CycleError(modules) =>
      s"Cycle loops detected in imports: ${modules.map(_.file.fileName)}"
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
          s"Semantic error: wrong AST"

      }

    case OutputError(_, err) =>
      Console.RED + err.showForConsole + Console.RESET
  }
}
