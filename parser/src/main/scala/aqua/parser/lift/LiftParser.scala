package aqua.parser.lift

import cats.Id
import cats.parse.{Parser, Parser0}

trait LiftParser[F[_]] {
  def lift[T](p: Parser[T]): Parser[F[T]]

  def lift0[T](p0: Parser0[T]): Parser0[F[T]]
}

object LiftParser {

  implicit class LiftParserOps[F[_]: LiftParser, T](parser: Parser[T]) {
    def lift: Parser[F[T]] = implicitly[LiftParser[F]].lift(parser)
  }

  implicit class LiftParser0Ops[F[_]: LiftParser, T](parser0: Parser0[T]) {
    def lift0: Parser0[F[T]] = implicitly[LiftParser[F]].lift0(parser0)
  }

  object Implicits {

    implicit object idLiftParser extends LiftParser[Id] {
      override def lift[T](p: Parser[T]): Parser[Id[T]] = p
      override def lift0[T](p0: Parser0[T]): Parser0[Id[T]] = p0
    }

  }
}
