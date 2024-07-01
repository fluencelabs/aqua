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

package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.Topology
import aqua.model.transform.topology.TopologyPath
import aqua.model.OnModel

import cats.Eval

// Parent == Seq, On
object SeqGroupBranch extends Before with After {
  override def toString: String = "<seq>/*"

  // If parent is seq, then before this node we are where previous node, if any, ends
  override def beforeOn(current: Topology): Eval[TopologyPath] =
    // Where we are after the previous node in the parent
    current.prevSibling
      .map(_.finallyOn) getOrElse super.beforeOn(current)

  override def afterOn(current: Topology): Eval[TopologyPath] =
    current.nextSibling.map(_.beginsOn) getOrElse afterParent(current)

}
