package aqua.model.func

import cats.data.State
import cats.syntax.flatMap.*

trait Counter[S] {
  self =>
  val get: State[S, Int]
  def add(i: Int): State[S, Unit]
  val incr: State[S, Int] = add(1) >> get

  def transformS[R](f: R => S, g: (R, S) => R): Counter[R] = new Counter[R] {
    override val get: State[R, Int] = self.get.transformS(f, g)

    override def add(i: Int): State[R, Unit] = self.add(i).transformS(f, g)
  }
}

object Counter {
  def apply[S](implicit counter: Counter[S]): Counter[S] = counter

  object Simple extends Counter[Int] {

    override val get: State[Int, Int] = State.get
    override def add(i: Int): State[Int, Unit] = State.modify[Int](_ + i)
  }
}
