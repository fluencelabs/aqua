package aqua.parse.lift

import cats.parse.{Parser ⇒ P}

import scala.language.implicitConversions

case class Span[T](startIndex: Int, endIndex: Int, value: T)

object Span {

  implicit object spanLiftParser extends LiftParser[Span] {

    override def lift[T](p: P[T]): P[Span[T]] =
      (P.index.with1 ~ p ~ P.index).map {
        case ((s, v), e) ⇒ Span(s, e, v)
      }
  }

}
