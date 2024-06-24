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

package aqua.model.transform.funcop

import aqua.model.OpModel

import cats.Eval
import cats.data.Chain
import cats.free.Cofree

/**
 * Base type for [[OpModel.Tree]] -> [[OpModel.Tree]] transformation
 */
trait OpTransform {

  /**
   * Transformation step
   * (node, child results) => node result
   */
  def folder: OpTransform.OpFolder

  def apply(tree: OpModel.Tree): Eval[OpModel.Tree] =
    Cofree.cata[Chain, OpModel, OpModel.Tree](tree)((op, children) =>
      folder
        .lift(op, children)
        .getOrElse(
          Eval.now(
            op.wrap(children.toList*)
          )
        )
    )
}

object OpTransform {
  type OpFolder = PartialFunction[(OpModel, Chain[OpModel.Tree]), Eval[OpModel.Tree]]
}
