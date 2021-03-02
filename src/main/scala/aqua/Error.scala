package aqua

import aqua.parser.lift.Span
import cats.data.NonEmptyList
import cats.parse.Parser.Expectation

sealed trait Error {
  def showForConsole(script: String): String
}

case class SyntaxError(offset: Int, expectations: NonEmptyList[Expectation]) extends Error {

  override def showForConsole(script: String): String =
    Span(offset, offset + 1)
      .focus(script, 3)
      .map(_.toConsoleStr(s"Syntax error, expected: ${expectations.toList.mkString(", ")}", Console.RED))
      .getOrElse(
        "(offset is beyond the script)"
      ) + "\n"
}

case class NamesError(span: Span, hint: String) extends Error {

  override def showForConsole(script: String): String =
    span
      .focus(script, 3)
      .map(_.toConsoleStr(hint, Console.YELLOW))
      .getOrElse("(offset is beyond the script)") + "\n"
}

case class GetTypeError(span: Span, hint: String) extends Error {

  override def showForConsole(script: String): String =
    span
      .focus(script, 3)
      .map(_.toConsoleStr(hint, Console.CYAN))
      .getOrElse("(offset is beyond the script)") + "\n"
}
