package aqua.parser.lift

import cats.Id
import cats.parse.{Parser, Parser0}

trait LiftParser[S[_]] {
  def lift[T](p: Parser[T]): Parser[S[T]]

  def lift0[T](p0: Parser0[T]): Parser0[S[T]]

  def wrapErr(e: Parser.Error): S[Parser.Error]
}

object LiftParser {

  implicit class LiftErrorOps[S[_]: LiftParser, T](e: Parser.Error) {
    def wrapErr: S[Parser.Error] = implicitly[LiftParser[S]].wrapErr(e)
  }

  implicit class LiftParserOps[S[_]: LiftParser, T](parser: Parser[T]) {
    def lift: Parser[S[T]] = implicitly[LiftParser[S]].lift(parser)
  }

  implicit class LiftParser0Ops[S[_]: LiftParser, T](parser0: Parser0[T]) {
    def lift0: Parser0[S[T]] = implicitly[LiftParser[S]].lift0(parser0)
  }

  object Implicits {

    implicit object idLiftParser extends LiftParser[Id] {
      override def lift[T](p: Parser[T]): Parser[Id[T]] = p
      override def lift0[T](p0: Parser0[T]): Parser0[Id[T]] = p0
      override def wrapErr(e: Parser.Error): Id[Parser.Error] = e
    }

  }
}
