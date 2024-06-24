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

import aqua.model.transform.topology.{PathFinder, Topology, TopologyPath}
import aqua.model.{OnModel, ValueModel}

import cats.Eval
import cats.data.Chain
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.monad.*
import cats.instances.tuple.*

trait Begins {

  def beginsOn(current: Topology): Eval[TopologyPath] = current.pathOn

  def pathBefore(current: Topology): Eval[Chain[ValueModel]] =
    (current.beforeOn, current.beginsOn).tupled
      .fproduct(PathFinder.findPath.tupled)
      .flatMap { case ((bef, beg), path) =>
        // Handle the case when we need to go through the relay, but miss the hop as it's the first
        // peer where we go, but there's no service calls there
        current.firstExecutesOn.map {
          case Some(where) if where != beg =>
            path ++ Topology.findRelayPathEnforcement(bef, beg)
          case _ => path
        }
      }
}
