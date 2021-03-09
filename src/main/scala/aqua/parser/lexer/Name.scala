package aqua.parser.lexer

import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Functor
import cats.parse.{Parser => P}
import cats.syntax.functor._

case class Name[F[_]](name: F[String]) extends Token[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = name.as(v)
}

object Name {

  def p[F[_]: LiftParser]: P[Name[F]] =
    `name`.lift.map(Name(_))
}
