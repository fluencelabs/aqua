package aqua.parse.lift

import cats.Id
import cats.parse.Parser

trait LiftParser[F[_]] {
  def lift[T](p: Parser[T]): Parser[F[T]]
}

object LiftParser {

  implicit class LiftParserOps[F[_]: LiftParser, T](parser: Parser[T]) {
    def lift: Parser[F[T]] = implicitly[LiftParser[F]].lift(parser)
  }

  object Implicits {

    implicit object idLiftParser extends LiftParser[Id] {
      override def lift[T](p: Parser[T]): Parser[Id[T]] = p
    }

  }
}
