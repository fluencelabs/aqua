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

package aqua.model.transform.topology

import aqua.model.OnModel
import aqua.model.ValueModel

import cats.kernel.Monoid
import cats.Show
import cats.data.Chain.:==

final case class TopologyPath(
  path: List[OnModel]
) extends AnyVal {
  def ::(on: OnModel): TopologyPath = TopologyPath(on :: path)

  // First `on` in the path
  def current: Option[OnModel] = path.headOption

  // Current peer id
  def peerId: Option[ValueModel] = current.map(_.peerId)

  // Path with the first `on` removed
  def previous: Option[TopologyPath] = path match {
    case _ :: tail => Some(TopologyPath(tail))
    case Nil => None
  }

  // Last relay in the current `on`
  def lastRelay: Option[ValueModel] = current.flatMap(_.via.lastOption)

  def reverse: TopologyPath = TopologyPath(path.reverse)

  def commonPrefix(other: TopologyPath): TopologyPath =
    TopologyPath(path.zip(other.path).takeWhile(_ == _).map(_._1))

  // Path of the first relay in the path
  def toRelay: TopologyPath = {
    def toRelayTailRec(
      currentPath: List[OnModel]
    ): List[OnModel] = currentPath match {
      case Nil => Nil
      case (on @ OnModel(_, other :== r, _)) :: tail =>
        on.copy(peerId = r, via = other) :: tail
      case _ :: tail => toRelayTailRec(tail)
    }

    TopologyPath(toRelayTailRec(path))
  }
}

object TopologyPath {

  given Monoid[TopologyPath] with {
    def empty: TopologyPath = TopologyPath(Nil)
    def combine(x: TopologyPath, y: TopologyPath): TopologyPath = TopologyPath(x.path ++ y.path)
  }

  val empty = Monoid[TopologyPath].empty

  given Show[TopologyPath] with {

    def show(t: TopologyPath): String =
      if (t.path.isEmpty) "empty"
      else t.path.map(_.toString).mkString(" -> ")
  }
}
