package aqua.parser.lexer

import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.syntax.functor._
import Token._
import cats.parse.{Parser => P}
import LiftParser._
import cats.syntax.comonad._
import cats.~>

case class EqOp[F[_]: Comonad](eq: F[Boolean]) extends Token[F] {
  override def as[T](v: T): F[T] = eq.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): EqOp[K] =
    copy(fk(eq))

  def value: Boolean = eq.extract
}

object EqOp {

  def p[F[_]: Comonad: LiftParser]: P[EqOp[F]] =
    (`eqs`.as(true).lift | `neq`.as(false).lift).map(EqOp(_))
}
