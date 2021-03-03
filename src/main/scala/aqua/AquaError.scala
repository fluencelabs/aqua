package aqua

import aqua.parser.lift.Span
import cats.data.NonEmptyList
import cats.parse.Parser.Expectation

sealed trait AquaError {
  def showForConsole(script: String): String
}

case class SyntaxError(offset: Int, expectations: NonEmptyList[Expectation]) extends AquaError {

  override def showForConsole(script: String): String =
    Span(offset, offset + 1)
      .focus(script, 3)
      .map(_.toConsoleStr(s"Syntax error, expected: ${expectations.toList.mkString(", ")}", Console.RED))
      .getOrElse(
        "(offset is beyond the script, syntax errors) " + Console.RED + expectations.toList.mkString(", ")
      ) + Console.RESET + "\n"
}

case class WalkerError(span: Span, hint: String) extends AquaError {

  override def showForConsole(script: String): String =
    span
      .focus(script, 3)
      .map(_.toConsoleStr(hint, Console.CYAN))
      .getOrElse("(Dup error, but offset is beyond the script)") + "\n"
}

case class GetTypeError(span: Span, hint: String) extends AquaError {

  override def showForConsole(script: String): String =
    span
      .focus(script, 3)
      .map(_.toConsoleStr(hint, Console.MAGENTA))
      .getOrElse("(Get type error, but offset is beyond the script)") + "\n"
}
