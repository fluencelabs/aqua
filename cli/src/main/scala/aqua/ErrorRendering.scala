package aqua

import aqua.compiler._
import aqua.files.FileModuleId
import aqua.io.AquaFileError
import aqua.parser.lift.FileSpan
import aqua.parser.{BlockIndentError, FuncReturnError, LexerError}
import aqua.semantics.{RulesViolated, WrongAST}
import cats.Show

object ErrorRendering {

  def showForConsole(span: FileSpan, message: String): String =
    span
      .focus(3)
      .map(
        _.toConsoleStr(
          message,
          Console.RED
        )
      )
      .getOrElse(
        "(offset is beyond the script, syntax errors) Error: " + Console.RED + message
          .mkString(", ")
      ) + Console.RESET + "\n"

  implicit val showError: Show[AquaError[FileModuleId, AquaFileError, FileSpan.F]] = Show.show {
    case ParserErr(err) =>
      err match {
        case BlockIndentError(indent, message) => showForConsole(indent._1, message)
        case FuncReturnError(point, message) => showForConsole(point._1, message)
        case LexerError(pe) =>
          // TODO: get FileSpan somehow
//          val fileSpan =
//            FileSpan(
//              name,
//              input,
//              Eval.later(LocationMap(input)),
//              Span(pe.failedAtOffset, pe.failedAtOffset + 1)
//            )
          pe.toString
      }
    case SourcesErr(err) =>
      Console.RED + err.showForConsole + Console.RESET
    case ResolveImportsErr(_, token, err) =>
      val span = token.unit._1
      showForConsole(span, s"Cannot resolve imports: ${err.showForConsole}")

    case ImportErr(token) =>
      val span = token.unit._1
      showForConsole(span, s"Cannot resolve import")
    case CycleError(modules) =>
      s"Cycle loops detected in imports: ${modules.map(_.file.getFileName)}"
    case CompileError(err) =>
      err match {
        case RulesViolated(token, message) =>
          token.unit._1
            .focus(2)
            .map(_.toConsoleStr(message, Console.CYAN))
            .getOrElse("(Dup error, but offset is beyond the script)") + "\n"
        case WrongAST(ast) =>
          s"Semantic error"

      }

    case OutputError(_, err) =>
      Console.RED + err.showForConsole + Console.RESET
  }
}
