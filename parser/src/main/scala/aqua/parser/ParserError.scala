package aqua.parser

import cats.parse.Parser
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
