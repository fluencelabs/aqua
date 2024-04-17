package aqua.parser.lift

import cats.Id
import cats.parse.{Parser, Parser0}

trait LiftParser[S[_]] {
  def lift[T](p: Parser[T]): Parser[S[T]]

  def lift0[T](p0: Parser0[T]): Parser0[S[T]]

  def wrapErr(e: Parser.Error): S[Parser.Error]
}

object LiftParser {

  def apply[S[_]](using lp: LiftParser[S]): LiftParser[S] = lp

  extension [S[_]: LiftParser, T](e: Parser.Error) {
    def wrapErr: S[Parser.Error] = LiftParser[S].wrapErr(e)
  }

  extension [S[_]: LiftParser, T](parser: Parser[T]) {
    def lift: Parser[S[T]] = LiftParser[S].lift(parser)
  }

  extension [S[_]: LiftParser, T](parser0: Parser0[T]) {
    def lift0: Parser0[S[T]] = LiftParser[S].lift0(parser0)
  }

  given LiftParser[Id] with {
    override def lift[T](p: Parser[T]): Parser[Id[T]] = p
    override def lift0[T](p0: Parser0[T]): Parser0[Id[T]] = p0
    override def wrapErr(e: Parser.Error): Id[Parser.Error] = e
  }
}
