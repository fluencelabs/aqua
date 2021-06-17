package aqua.model.topology

import aqua.model.func.body.{MetaTag, OnTag, SeqGroupTag}
import cats.data.Chain
import cats.free.Cofree
import wvlet.log.LogSupport

case class Location(path: List[ChainZipper[Topology.Tree]] = Nil) extends LogSupport {
  def down(h: ChainZipper[Topology.Tree]): Location = copy(h :: path)

  def lastOn: Option[OnTag] = pathOn.lastOption

  def firstOn: Option[OnTag] = pathOn.headOption

  lazy val pathOn: Chain[OnTag] = Chain
    .fromSeq(path.map(_.current.head).collect {
      case o: OnTag =>
        o
      case MetaTag(false, _, o: OnTag) => o
    })
    .reverse

  def lastLeftSeq: Option[(ChainZipper[Topology.Tree], Location)] =
    path match {
      case (cz @ ChainZipper(
            prev,
            Cofree(_: SeqGroupTag | MetaTag(false, _, _: SeqGroupTag), _),
            _
          )) :: tail if prev.nonEmpty =>
        cz.moveLeft.map(_ -> Location(tail))
      case _ :: tail =>
        Location(tail).lastLeftSeq
      case Nil => None
    }

  def lastRightSeq: Option[(ChainZipper[Topology.Tree], Location)] =
    path match {
      case (cz @ ChainZipper(
            _,
            Cofree(_: SeqGroupTag | MetaTag(false, _, _: SeqGroupTag), _),
            next
          )) :: tail if next.nonEmpty =>
        cz.moveRight.map(_ -> Location(tail))
      case _ :: tail => Location(tail).lastRightSeq
      case Nil => None
    }
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
