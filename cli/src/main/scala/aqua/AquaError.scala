package aqua

import aqua.parser.lift.Span
import cats.Eval
import cats.data.NonEmptyList
import cats.parse.LocationMap
import cats.parse.Parser.Expectation

sealed trait AquaError {
  def showForConsole(script: String): String
}

case class CustomSyntaxError(span: Span, message: String) extends AquaError {

  override def showForConsole(script: String): String =
    span
      .focus(Eval.later(LocationMap(script)), 2)
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
}

case class SyntaxError(offset: Int, expectations: NonEmptyList[Expectation]) extends AquaError {

  override def showForConsole(script: String): String =
    Span(offset, offset + 1)
      .focus(Eval.later(LocationMap(script)), 2)
      .map(
        _.toConsoleStr(
          s"Syntax error, expected: ${expectations.toList.mkString(", ")}",
          Console.RED
        )
      )
      .getOrElse(
        "(offset is beyond the script, syntax errors) " + Console.RED + expectations.toList
          .mkString(", ")
      ) + Console.RESET + "\n"
}

case class CompilerError(span: Span, hint: String) extends AquaError {

  override def showForConsole(script: String): String =
    span
      .focus(Eval.later(LocationMap(script)), 1)
      .map(_.toConsoleStr(hint, Console.CYAN))
      .getOrElse("(Dup error, but offset is beyond the script)") + "\n"
}
