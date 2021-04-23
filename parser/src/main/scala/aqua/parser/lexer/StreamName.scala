package aqua.parser.lexer

import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Comonad
import cats.parse.{Parser => P}
import cats.syntax.functor._
import cats.syntax.comonad._

case class StreamName[F[_]: Comonad](name: F[String]) extends Token[F] {
  override def as[T](v: T): F[T] = name.as(v)

  def value: String = name.extract
}

object StreamName {

  def p[F[_]: LiftParser: Comonad]: P[StreamName[F]] =
    `$stream`.lift.map(StreamName(_))
}
