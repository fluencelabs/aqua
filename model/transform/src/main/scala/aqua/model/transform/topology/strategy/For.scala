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
import aqua.model.{NextModel, OnModel}

import cats.Eval
import cats.syntax.apply.*
import cats.data.Chain.*
import cats.data.State
import cats.syntax.option.*
import cats.instances.list.*
import cats.syntax.foldable.*
import cats.syntax.applicative.*
import cats.syntax.functor.*

object For extends Begins {
  override def toString: String = "<for>"

  // Optimization: get all the path inside the For block out of the block, to avoid repeating
  // hops for every For iteration
  override def beginsOn(current: Topology): Eval[TopologyPath] =
    // Skip `next` child because its `beginsOn` depends on `this.beginsOn`, see [bug LNG-149]
    (current.forModel zip firstNotNextChild(current).map(_.beginsOn)).map {
      case (model, childBeginsOn) =>
        for {
          child <- childBeginsOn
          // Take path until this for's iterator is used
          path <- child.reverse.path
            .foldM(TopologyPath.empty)((acc, on) =>
              State
                .get[Boolean]
                .flatMap(found =>
                  if (found) acc.pure // Short circuit
                  else
                    (acc.path, on) match {
                      case (_, OnModel(_, r, _)) if r.exists(_.usesVarNames.contains(model.item)) =>
                        State.set(true).as(acc)
                      case (OnModel(_, r @ (r0 ==: _), _) :: _, OnModel(p, _, _))
                          if p.usesVarNames.contains(model.item) =>
                        // This is to take the outstanding relay and force moving there
                        State.set(true).as(OnModel(r0, r) :: acc)
                      case _ => (on :: acc).pure
                    }
                )
            )
            .runA(false)
        } yield path
    } getOrElse super.beginsOn(current)

  /**
   * Find first child that is not `next`
   */
  private def firstNotNextChild(current: Topology): Option[Topology] = for {
    first <- current.firstChild
    notNext <- first.cursor.op match {
      case _: NextModel => first.nextSibling
      case _ => first.some
    }
  } yield notNext
}
