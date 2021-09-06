package aqua.parser.lexer

import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Comonad
import cats.parse.{Parser => P}
import cats.syntax.functor._
import cats.syntax.comonad._
import cats.~>

case class Name[F[_]: Comonad](name: F[String]) extends Token[F] {
  override def as[T](v: T): F[T] = name.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): Name[K] = copy(fk(name))

  def value: String = name.extract
}

object Name {

  type As[F[_]] = (Name[F], Option[Name[F]])

  def p[F[_]: LiftParser: Comonad]: P[Name[F]] =
    `name`.lift.map(Name(_))

  def dotted[F[_]: LiftParser: Comonad]: P[Name[F]] =
    ((`Class`.repSep(`.`).map(_.toList.mkString(".")) ~ `.`).?.with1 ~ `name`).string.lift
      .map(Name(_))

  def nameAs[F[_]: LiftParser: Comonad]: P[As[F]] =
    asOpt(p[F])
}
