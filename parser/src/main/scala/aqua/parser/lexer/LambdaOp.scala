package aqua.parser.lexer

import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import cats.data.NonEmptyList
import cats.parse.{Numbers, Parser as P, Parser0 as P0}
import cats.syntax.comonad.*
import cats.syntax.functor.*
import cats.{Comonad, Functor}

import scala.language.postfixOps
import cats.~>
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

sealed trait LambdaOp[F[_]] extends Token[F] {
  def mapK[K[_]: Comonad](fk: F ~> K): LambdaOp[K]
}

case class IntoField[F[_]: Comonad](name: F[String]) extends LambdaOp[F] {
  override def as[T](v: T): F[T] = name.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): LambdaOp[K] = copy(fk(name))

  def value: String = name.extract
}

case class IntoIndex[F[_]: Comonad](idx: F[Int]) extends LambdaOp[F] {
  override def as[T](v: T): F[T] = idx.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): IntoIndex[K] = copy(fk(idx))

  def value: Int = idx.extract
}

case class IntoArray[F[_]: Functor](override val unit: F[Unit]) extends LambdaOp[F] {
  override def as[T](v: T): F[T] = unit.as(v)

  override def mapK[K[_]: Comonad](fk: F ~> K): IntoArray[K] = copy(fk(unit))
}

object LambdaOp {

  private val parseField: P[LambdaOp[Span.F]] =
    (`.` *> `name`).lift.map(IntoField(_))

  private val parseArr: P[LambdaOp[Span.F]] = `*`.lift.map(IntoArray(_))

  private val nonNegativeIntP0: P0[Int] =
    Numbers.nonNegativeIntString.map(_.toInt).?.map(_.getOrElse(0))

  private val parseIdx: P[LambdaOp[Span.F]] =
    (exclamation *> nonNegativeIntP0).lift.map(IntoIndex(_))

  private val parseOp: P[LambdaOp[Span.F]] =
    P.oneOf(parseField.backtrack :: parseArr :: parseIdx :: Nil)

  val ops: P[NonEmptyList[LambdaOp[Span.F]]] =
    parseOp.rep

}
