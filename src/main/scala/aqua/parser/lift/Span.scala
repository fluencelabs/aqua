package aqua.parser.lift

import cats.Comonad
import cats.parse.{Parser ⇒ P}
import cats.free.Free

import scala.language.implicitConversions

case class Span[T](startIndex: Int, endIndex: Int, value: T)

object Span {

  implicit object spanComonad extends Comonad[Span] {
    override def extract[A](x: Span[A]): A = x.value

    override def coflatMap[A, B](fa: Span[A])(f: Span[A] ⇒ B): Span[B] = fa.copy(value = f(fa))

    override def map[A, B](fa: Span[A])(f: A ⇒ B): Span[B] = fa.copy(value = f(fa.value))
  }

  implicit object spanLiftParser extends LiftParser[Span] {

    override def lift[T](p: P[T]): P[Span[T]] =
      (P.index.with1 ~ p ~ P.index).map {
        case ((s, v), e) ⇒ Span(s, e, v)
      }
  }

}
