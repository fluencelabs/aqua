package aqua.parser.lexer

import Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.{Comonad, Functor}
import cats.data.NonEmptyList
import cats.parse.{Parser => P}
import cats.syntax.functor._
import cats.syntax.comonad._

sealed trait LambdaOp[F[_]] extends Token[F]

case class IntoField[F[_]: Comonad](name: F[String]) extends LambdaOp[F] {
  override def as[T](v: T): F[T] = name.as(v)

  def value: String = name.extract
}

case class IntoArray[F[_]: Functor](override val unit: F[Unit]) extends LambdaOp[F] {
  override def as[T](v: T): F[T] = unit.as(v)
}

object LambdaOp {
  private def parseField[F[_]: LiftParser: Comonad]: P[LambdaOp[F]] = (`.` *> `name`).lift.map(IntoField(_))
  private def parseArr[F[_]: LiftParser: Comonad]: P[LambdaOp[F]] = `*`.lift.map(IntoArray(_))
  private def parseOp[F[_]: LiftParser: Comonad]: P[LambdaOp[F]] = P.oneOf(parseField.backtrack :: parseArr :: Nil)

  def ops[F[_]: LiftParser: Comonad]: P[NonEmptyList[LambdaOp[F]]] =
    parseOp.rep

}
