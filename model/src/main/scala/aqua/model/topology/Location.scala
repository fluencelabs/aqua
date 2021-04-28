package aqua.model.topology

import aqua.model.ValueModel
import aqua.model.func.body.{OnTag, SeqGroupTag}
import cats.data.Chain
import cats.free.Cofree

case class Location(path: List[ChainZipper[Topology.Tree]] = Nil) {
  def down(h: ChainZipper[Topology.Tree]): Location = copy(h :: path)

  def lastOn: Option[OnTag] = path.map(_.current.head).collectFirst { case o: OnTag =>
    o
  }

  def pathOn: List[OnTag] = path.map(_.current.head).collect { case o: OnTag =>
    o
  }

  def pathViaChain: Chain[ValueModel] = Chain.fromSeq(
    path
      .map(_.current.head)
      .collectFirst { case t: OnTag =>
        t.via.toList
      }
      .toList
      .flatten
  )

  def lastLeftSeq: Option[(ChainZipper[Topology.Tree], Location)] =
    path match {
      case (cz @ ChainZipper(prev, Cofree(_: SeqGroupTag, _), _)) :: tail if prev.nonEmpty =>
        cz.moveLeft.map(_ -> Location(tail))
      case _ :: tail => Location(tail).lastLeftSeq
      case Nil => None
    }

  def lastRightSeq: Option[(ChainZipper[Topology.Tree], Location)] =
    path match {
      case (cz @ ChainZipper(_, Cofree(_: SeqGroupTag, _), next)) :: tail if next.nonEmpty =>
        cz.moveRight.map(_ -> Location(tail))
      case _ :: tail => Location(tail).lastRightSeq
      case Nil => None
    }

  path.collectFirst {
    case ChainZipper(prev, Cofree(_: SeqGroupTag, _), _) if prev.nonEmpty => prev.lastOption
  }.flatten
}

object Location {

  object Matchers {

    object /: {

      def unapply(loc: Location): Option[(ChainZipper[Topology.Tree], Location)] =
        loc.path match {
          case h :: tail => Some(h -> Location(tail))
          case _ => None
        }
    }
  }
}
