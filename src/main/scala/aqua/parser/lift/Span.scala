package aqua.parser.lift

import cats.Comonad
import cats.parse.{Parser ⇒ P}

import scala.language.implicitConversions

case class Span(startIndex: Int, endIndex: Int)

object Span {
  type F[T] = (Span, T)

  implicit object spanComonad extends Comonad[F] {
    override def extract[A](x: F[A]): A = x._2

    override def coflatMap[A, B](fa: F[A])(f: F[A] ⇒ B): F[B] = fa.copy(_2 = f(fa))

    override def map[A, B](fa: F[A])(f: A ⇒ B): F[B] = fa.copy(_2 = f(fa._2))
  }

  implicit object spanLiftParser extends LiftParser[F] {

    override def lift[T](p: P[T]): P[F[T]] =
      (P.index.with1 ~ p ~ P.index).map {
        case ((s, v), e) ⇒ (Span(s, e), v)
      }
  }

}
