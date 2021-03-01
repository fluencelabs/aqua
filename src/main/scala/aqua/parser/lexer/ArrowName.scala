package aqua.parser.lexer

import aqua.parser.lexer.Token.`name`
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Functor
import cats.syntax.functor._
import cats.parse.{Parser => P}

case class ArrowName[F[_]](name: F[String]) extends Token[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = name.as(v)
}

object ArrowName {

  def an[F[_]: LiftParser]: P[ArrowName[F]] =
    `name`.lift.map(ArrowName(_))
}
