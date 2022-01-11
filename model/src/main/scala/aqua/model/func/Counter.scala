package aqua.model.func

import cats.data.State
import cats.syntax.flatMap.*

trait Counter[S] {
  self =>
  val incr: State[S, Int]

  def transformS[R](f: R => S, g: (R, S) => R): Counter[R] = new Counter[R] {

    override val incr: State[R, Int] =
      self.incr.transformS(f, g)
  }
}

object Counter {
  def apply[S](implicit counter: Counter[S]): Counter[S] = counter

  object Simple extends Counter[Int] {

    override val incr: State[Int, Int] =
      State.modify[Int](_ + 1) >> State.get
  }
}
