package aqua.model.inline.state

import cats.data.State
import cats.syntax.flatMap.*

/**
 * Monotonic counter, stored within the State monad
 *
 * @tparam S
 *   State
 */
trait Counter[S] {
  self =>
  // Get counter
  val get: State[S, Int]

  // Increment by i
  def add(i: Int): State[S, Unit]

  // Increment by 1 and get
  val incr: State[S, Int] = add(1) >> get

  // Change state [[S]] to [[R]]
  def transformS[R](f: R => S, g: (R, S) => R): Counter[R] = new Counter[R] {
    override val get: State[R, Int] = self.get.transformS(f, g)

    override def add(i: Int): State[R, Unit] = self.add(i).transformS(f, g)
  }
}

object Counter {
  def apply[S](implicit counter: Counter[S]): Counter[S] = counter

  given Simple: Counter[Int] = new Counter[Int] {

    override val get: State[Int, Int] = State.get

    override def add(i: Int): State[Int, Unit] = State.modify[Int](_ + i)
  }
}
