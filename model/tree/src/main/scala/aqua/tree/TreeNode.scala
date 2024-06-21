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

package aqua.tree

import cats.data.Chain
import cats.data.Chain.*
import cats.free.Cofree
import cats.Eval

trait TreeNode[T <: TreeNode[T]] {
  self: T =>
  type Tree = Cofree[Chain, T]

  lazy val leaf: Tree = Cofree(self, Eval.now(Chain.empty))

  def wrap(children: Tree*): Tree = wrap(Chain.fromSeq(children))

  def wrap(children: List[Tree]): Tree = wrap(Chain.fromSeq(children))

  def wrap(children: Chain[Tree]): Tree = Cofree(self, Eval.now(children))

  protected def wrapNonEmpty(children: Chain[Tree], empty: Tree): Tree =
    children match {
      case Chain.nil => empty
      case x ==: Chain.nil => x
      // Do not use `wrap` here as children
      // could redefine `wrap` through this method
      case _ => Cofree(self, Eval.now(children))
    }

}
