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

package aqua.helpers.tree

import cats.free.Cofree
import cats.syntax.foldable.*
import cats.syntax.show.*
import cats.{Eval, Show, Traverse}

object Tree {

  def show[F[_]: Traverse, A: Show](
    what: Cofree[F, A]
  ): String =
    Cofree
      .cata[F, A, List[String]](what) { case (head, tail) =>
        Eval.later {
          val children = tail.combineAll.map("| " + _)
          val parent = head.show

          if (children.isEmpty) List(parent)
          else (parent + ":") +: children
        }
      }
      .value
      .mkString("\n")

}
