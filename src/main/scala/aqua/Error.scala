package aqua

import aqua.parser.lift.Span
import cats.data.NonEmptyList
import cats.parse.Parser.Expectation

sealed trait Error {
  def showForConsole(script: String): String
}

case class SyntaxError(offset: Int, expectations: NonEmptyList[Expectation]) extends Error {

  // TODO print expectations
  override def showForConsole(script: String): String =
    Span(offset, offset + 1)
      .focus(script, 3)
      .map(_.toConsoleStr(Console.RED))
      .getOrElse(
        "(offset is beyond the script)"
      ) ++ s"\n${Console.RED}Syntax error${Console.RESET}, expected: ${expectations.toList.mkString(", ")}\n"
}

case class NamesError(span: Span, hint: String) extends Error {

  override def showForConsole(script: String): String =
    span
      .focus(script, 3)
      .map(_.toConsoleStr(Console.YELLOW))
      .getOrElse("(offset is beyond the script)") ++ s"\n${Console.YELLOW}${hint}${Console.RESET}\n"
}
