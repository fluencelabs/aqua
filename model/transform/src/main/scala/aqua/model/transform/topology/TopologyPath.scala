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

  def current: Option[OnModel] = path.headOption

  def peerId: Option[ValueModel] = current.map(_.peerId)

  def previous: Option[TopologyPath] = path match {
    case _ :: tail => Some(TopologyPath(tail))
    case Nil => None
  }

  def lastRelay: Option[ValueModel] = current.flatMap(_.via.lastOption)

  def reverse: TopologyPath = TopologyPath(path.reverse)

  def commonPrefix(other: TopologyPath): TopologyPath =
    TopologyPath(path.zip(other.path).takeWhile(_ == _).map(_._1))

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
