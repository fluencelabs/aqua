package aqua.parser.lexer

import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Comonad
import cats.parse.{Parser => P}
import cats.syntax.functor._
import cats.syntax.comonad._
import cats.~>

case class Ability[F[_]: Comonad](name: F[String]) extends Token[F] {
  override def as[T](v: T): F[T] = name.as(v)

  def mapK[K[_]: Comonad](fk: F ~> K): Ability[K] = copy(fk(name))

  def value: String = name.extract
}

object Ability {
  type As[F[_]] = (Ability[F], Option[Ability[F]])

  def ab[F[_]: LiftParser: Comonad]: P[Ability[F]] =
    `Class`.lift.map(Ability(_))

  def dotted[F[_]: LiftParser: Comonad]: P[Ability[F]] =
    P.repSep(`Class`, `.`).map(_.toList.mkString(".")).lift.map(Ability(_))

  def abAs[F[_]: LiftParser: Comonad]: P[As[F]] =
    asOpt(ab[F])
}
