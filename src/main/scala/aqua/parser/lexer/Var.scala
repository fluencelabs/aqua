package aqua.parser.lexer

import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Functor
import cats.parse.{Parser => P}
import cats.syntax.functor._

case class Var[F[_]](name: F[String]) extends Token[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = name.as(v)
}

object Var {

  def p[F[_]: LiftParser]: P[Var[F]] =
    `name`.lift.map(Var(_))
}
