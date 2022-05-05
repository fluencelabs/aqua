package aqua.parser

import cats.parse.Parser
import cats.parse.Parser.Expectation
import cats.parse.Parser.Expectation.{FailWith, InRange, OneOfStr, WithContext}
import cats.~>

trait ParserError[F[_]] {
  def mapK[K[_]](fk: F ~> K): ParserError[K]
}

case class LexerError[F[_]](err: F[Parser.Error]) extends ParserError[F] {
  def mapK[K[_]](fk: F ~> K): LexerError[K] = copy(fk(err))
}

case class BlockIndentError[F[_]](indent: F[String], message: String) extends ParserError[F] {

  def mapK[K[_]](fk: F ~> K): BlockIndentError[K] =
    copy(fk(indent))
}

case class ArrowReturnError[F[_]](point: F[Unit], message: String) extends ParserError[F] {

  def mapK[K[_]](fk: F ~> K): ArrowReturnError[K] =
    copy(fk(point))
}

object ParserError {

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
}
