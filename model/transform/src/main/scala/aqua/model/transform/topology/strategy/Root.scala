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
import aqua.model.OnModel

import cats.Eval

object Root extends Before with Ends with After {
  override def toString: String = "<root>"

  override def beforeOn(current: Topology): Eval[TopologyPath] = current.beginsOn

  override def endsOn(current: Topology): Eval[TopologyPath] = current.pathOn

  override def afterOn(current: Topology): Eval[TopologyPath] = current.pathOn

  override def forceExit(current: Topology): Eval[ExitStrategy] = Eval.now(ExitStrategy.Empty)
}
