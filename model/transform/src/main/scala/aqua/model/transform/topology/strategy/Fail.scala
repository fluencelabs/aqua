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
import aqua.model.transform.topology.Topology.ExitStrategy
import aqua.model.ValueModel
import aqua.model.{OnModel, XorModel}

import cats.data.Chain
import cats.Eval
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.option.*
import cats.syntax.applicative.*

object Fail extends Begins {

  override def pathBefore(current: Topology): Eval[Chain[ValueModel]] =
    for {
      path <- super.pathBefore(current)
      begins <- current.beginsOn
      // Get last hop to final peer
      // if it is not in the path
      // TODO: Add option to enforce last hop to [[PathFinder]]
      hop = begins.peerId.filterNot(peer => path.lastOption.contains(peer) || path.isEmpty)
    } yield path ++ Chain.fromOption(hop)
}
