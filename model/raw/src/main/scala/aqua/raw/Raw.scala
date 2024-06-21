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

package aqua.raw

import aqua.raw.RawPart.contextPart
import aqua.raw.ops.{FuncOp, RawTag}

import cats.Semigroup
import cats.syntax.semigroup.*

trait Raw

object Raw {

  def error(log: String): Raw = Empty(log)
  def empty(log: String): Raw = Empty(log)

  case class Empty(log: String) extends Raw

  given Semigroup[Raw] with {

    override def combine(x: Raw, y: Raw): Raw =
      (x, y) match {
        case (l: FuncOp, r: FuncOp) => FuncOp(l.tree |+| r.tree)

        case (l: Empty, r: Empty) => Empty(l.log + " |+| " + r.log)
        case (_: Empty, r) => r
        case (l, _: Empty) => l

        case (l, r) => contextPart(l) |+| contextPart(r)
      }
  }
}
