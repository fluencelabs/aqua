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

import cats.data.{Reader, State}

/**
 * Representation that `S` contains configuration for inlining
 */
trait Config[S] {
  self =>

  /**
   * Flag that disables error propagation mechanics in inlined code
   */
  def noErrorPropagation: Reader[S, Boolean]

  final def transform[R](f: R => S): Config[R] = new Config[R] {

    override def noErrorPropagation: Reader[R, Boolean] =
      self.noErrorPropagation.local(f)
  }
}

object Config {
  case class Values(noErrorPropagation: Boolean = false)

  object Values {
    lazy val default: Values = Values()
  }

  given Config[Values] = new Config[Values] {

    override def noErrorPropagation: Reader[Values, Boolean] =
      Reader(_.noErrorPropagation)
  }

  def apply[S: Config]: Config[S] =
    implicitly[Config[S]]

  def noErrorPropagation[S: Config]: Reader[S, Boolean] =
    Config[S].noErrorPropagation
}
