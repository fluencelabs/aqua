/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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
