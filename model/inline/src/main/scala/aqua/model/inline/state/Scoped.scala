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
import cats.syntax.apply.*
import cats.syntax.functor.*

/**
 * Common transformations to make an isolated scope for the state [[S]]
 */
trait Scoped[S] {

  /**
   * Clear the state, run [[scoped]], then recover the initial state
   * @param scoped What to run with empty [[S]]
   * @return Value returned by [[scoped]]
   */
  def scope[T](scoped: State[S, T]): State[S, T] =
    for {
      r <- purge
      t <- scoped
      _ <- set(r)
    } yield t

  protected def purge: State[S, S]

  protected def set(s: S): State[S, Unit]

  protected final def purgeR[R](f: R => S, g: (R, S) => R): State[R, R] =
    (State.get[R], purge.transformS(f, g)).mapN(g)

  protected final def setR[R](f: R => S, g: (R, S) => R)(r: R): State[R, Unit] =
    set(f(r)).transformS(f, g)

  private final def get: State[S, S] = for {
    s <- purge
    _ <- set(s)
  } yield s

}
