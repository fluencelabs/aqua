package aqua.helpers.syntax

import scala.annotation.tailrec

object list {

  extension [A](l: List[A]) {

    def updateFirst[B >: A](p: A => Boolean, f: A => B): List[B] = {
      @tailrec
      def update(left: List[B], right: List[A]): List[B] =
        right match {
          case a :: tail if p(a) => left.reverse ::: f(a) :: tail
          case a :: tail => update(a :: left, tail)
          case Nil => left.reverse
        }

      update(Nil, l)
    }
  }
}
