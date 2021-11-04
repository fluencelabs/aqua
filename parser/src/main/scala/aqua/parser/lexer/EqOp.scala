package aqua.parser.lexer

import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.syntax.functor.*
import Token.*
import cats.parse.Parser as P
import LiftParser.*
import cats.syntax.comonad.*
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

case class EqOp[F[_]: Comonad](eq: F[Boolean]) extends Token[F] {
  override def as[T](v: T): F[T] = eq.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): EqOp[K] =
    copy(fk(eq))

  def value: Boolean = eq.extract
}

object EqOp {

  val p: P[EqOp[Span.S]] =
    (`eqs`.as(true).lift | `neq`.as(false).lift).map(EqOp(_))
}
