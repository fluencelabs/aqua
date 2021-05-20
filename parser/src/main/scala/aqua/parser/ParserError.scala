package aqua.parser

import cats.parse.Parser

trait ParserError[F[_]]

case class LexerError[F[_]](err: Parser.Error) extends ParserError[F]
case class BlockIndentError[F[_]](indent: F[String], message: String) extends ParserError[F]
case class FuncReturnError[F[_]](point: F[Unit], message: String) extends ParserError[F]
