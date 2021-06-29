package aqua.compiler

import aqua.parser.lift.FileSpan
import cats.data.NonEmptyList
import cats.parse.Parser.Expectation

sealed trait AquaError {
  def showForConsole: String
}

case class CustomSyntaxError(span: FileSpan, message: String) extends AquaError {

  override def showForConsole: String =
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
}

case class SyntaxError(span: FileSpan, expectations: NonEmptyList[Expectation]) extends AquaError {

  override def showForConsole: String =
    span
      .focus(3)
      .map(spanFocus =>
        spanFocus.toConsoleStr(
          s"Syntax error, expected: ${expectations.toList.mkString(", ")}",
          Console.RED
        )
      )
      .getOrElse(
        "(offset is beyond the script, syntax errors) " + Console.RED + expectations.toList
          .mkString(", ")
      ) + Console.RESET + "\n"
}
