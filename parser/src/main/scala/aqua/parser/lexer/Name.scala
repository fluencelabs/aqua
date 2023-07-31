package aqua.parser.lexer

import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Comonad
import cats.parse.{Parser => P}
import cats.syntax.functor._
import cats.syntax.comonad._
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class Name[F[_]: Comonad](name: F[String]) extends Token[F] {
  override def as[T](v: T): F[T] = name.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): Name[K] = copy(fk(name))

  def rename(newName: String): Name[F] = copy(name.as(newName))

  def value: String = name.extract

  override def toString() = value
}

object Name {

  type As[F[_]] = (Name[F], Option[Name[F]])

  val p: P[Name[Span.S]] =
    `name`.lift.map(Name(_))

  val variable: P[Name[Span.S]] =
    (name | Class).lift.map(Name(_))

  val upper: P[Name[Span.S]] =
    NAME.lift.map(Name(_))

  val nameAs: P[As[Span.S]] =
    asOpt(p)
}
