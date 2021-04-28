package aqua.model.topology

import aqua.model.ValueModel
import aqua.model.func.body.OnTag
import cats.data.Chain

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
