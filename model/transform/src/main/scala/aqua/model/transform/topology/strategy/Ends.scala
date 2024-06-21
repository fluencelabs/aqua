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

import aqua.model.transform.topology.{PathFinder, Topology}
import aqua.model.transform.topology.TopologyPath
import aqua.model.transform.topology.Topology.ExitStrategy
import aqua.model.OnModel

import cats.Eval

trait Ends {

  def endsOn(current: Topology): Eval[TopologyPath] =
    current.beginsOn

  private def childFinally(
    current: Topology,
    child: Topology => Option[Topology]
  ): Eval[TopologyPath] =
    child(current).map(lc =>
      lc.forceExit.flatMap {
        case ExitStrategy.Empty => lc.endsOn
        case ExitStrategy.ToRelay => lc.pathOn.map(_.toRelay)
        case ExitStrategy.Full => current.afterOn
      }
    ) getOrElse current.beginsOn

  protected def lastChildFinally(current: Topology): Eval[TopologyPath] =
    childFinally(current, _.lastChild)

  protected def firstChildFinally(current: Topology): Eval[TopologyPath] =
    childFinally(current, _.firstChild)
}
