package aqua.parser.lexer

import Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Functor
import cats.data.NonEmptyList
import cats.parse.{Parser => P}
import cats.syntax.functor._

sealed trait LambdaOp[F[_]] extends Token[F]

case class IntoField[F[_]](name: F[String]) extends LambdaOp[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = name.as(v)

}

case class IntoArray[F[_]](f: F[Unit]) extends LambdaOp[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = f.as(v)
}

object LambdaOp {
  private def parseField[F[_]: LiftParser]: P[LambdaOp[F]] = (`.` *> `name`).lift.map(IntoField(_))
  private def parseArr[F[_]: LiftParser]: P[LambdaOp[F]] = `*`.lift.map(IntoArray(_))
  private def parseOp[F[_]: LiftParser]: P[LambdaOp[F]] = P.oneOf(parseField.backtrack :: parseArr :: Nil)

  def ops[F[_]: LiftParser]: P[NonEmptyList[LambdaOp[F]]] =
    parseOp.rep

}
