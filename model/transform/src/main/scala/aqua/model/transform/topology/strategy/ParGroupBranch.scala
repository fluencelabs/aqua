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
import aqua.model.transform.topology.Topology.ExitStrategy
import aqua.model.{OnModel, ValueModel}

import cats.Eval
import cats.data.Chain

// Parent == Par
object ParGroupBranch extends Ends with After {
  override def toString: String = "<par>/*"

  override def forceExit(current: Topology): Eval[ExitStrategy] =
    current.cursor
      .exportsUsedLaterFilter(
        _.op match {
          // This feels like a hack:
          // We suppose that `on` with Relay strategy
          // does not want to generate return transitions
          // because of it's exports.
          // This is used for `parseq` implementation.
          // We could not use `forceExit` of childs here
          // because it would cause infinite recursion.
          case OnModel(_, _, Some(OnModel.ReturnStrategy.Relay)) => false
          case _ => true
        }
      )
      .map(used =>
        if (used) ExitStrategy.Full
        else ExitStrategy.Empty
      )

  override def afterOn(current: Topology): Eval[TopologyPath] =
    afterParent(current)

  override def pathAfter(current: Topology): Eval[Chain[ValueModel]] =
    pathAfterAndPingNext(current)

  override def endsOn(current: Topology): Eval[TopologyPath] = current.beforeOn
}
