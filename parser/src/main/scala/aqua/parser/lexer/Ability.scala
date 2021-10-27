package aqua.parser.lexer

import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import cats.Comonad
import cats.parse.Parser as P
import cats.syntax.functor.*
import cats.syntax.comonad.*
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class Ability[F[_]: Comonad](name: F[String]) extends Token[F] {
  override def as[T](v: T): F[T] = name.as(v)

  def mapK[K[_]: Comonad](fk: F ~> K): Ability[K] = copy(fk(name))

  def value: String = name.extract
}

object Ability {
  type As[F[_]] = (Ability[F], Option[Ability[F]])

  val ab: P[Ability[Span.F]] =
    `Class`.lift.map(Ability(_))

  val dotted: P[Ability[Span.F]] =
    P.repSep(`Class`, `.`).map(_.toList.mkString(".")).lift.map(Ability(_))

  val abAs: P[As[Span.F]] =
    asOpt(ab)
}
