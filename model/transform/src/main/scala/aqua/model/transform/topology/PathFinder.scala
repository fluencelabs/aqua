package aqua.model.transform.topology

import aqua.model.ValueModel
import aqua.model.OnModel
import cats.data.Chain
import cats.data.Chain.{:==, ==:, nil}
import scribe.Logging

import scala.annotation.tailrec

object PathFinder extends Logging {

  /**
   * Finds the path – chain of peers to visit to get from [[fromOn]] to [[toOn]]
   * @param fromOn
   *   Previous location
   * @param toOn
   *   Next location
   * @return
   *   Chain of peers to visit in between
   */
  def findPath(fromOn: TopologyPath, toOn: TopologyPath): Chain[ValueModel] =
    findPath(
      Chain.fromSeq(fromOn.path.reverse),
      Chain.fromSeq(toOn.path.reverse),
      fromOn.peerId,
      toOn.peerId
    )

  def findPathEnforce(fromOn: TopologyPath, toOn: TopologyPath): Chain[ValueModel] = {
    val path = findPath(
      Chain.fromSeq(fromOn.path.reverse),
      Chain.fromSeq(toOn.path.reverse),
      fromOn.peerId,
      toOn.peerId
    )

    toOn.peerId.fold(path)(p => path :+ p)
  }

  private def findPath(
    fromOn: Chain[OnModel],
    toOn: Chain[OnModel],
    fromPeer: Option[ValueModel],
    toPeer: Option[ValueModel]
  ): Chain[ValueModel] = {
    logger.trace(s"FROM ON: $fromOn")
    logger.trace(s"TO ON: $toOn")

    val (from, to) = skipCommonPrefix(fromOn, toOn)
    val fromFix =
      if (from.isEmpty && fromPeer != toPeer) Chain.fromOption(fromOn.lastOption)
      else from
    val toFix =
      if (to.isEmpty && fromPeer != toPeer) Chain.fromOption(toOn.lastOption)
      else to

    logger.trace("FIND PATH FROM | " + fromFix)
    logger.trace("            TO | " + toFix)

    val fromTo = fromFix.reverse.flatMap(_.via.reverse) ++ toFix.flatMap(_.via)
    logger.trace(s"FROM TO: $fromTo")

    val toOptimize = Chain.fromOption(fromPeer) ++ fromTo ++ Chain.fromOption(toPeer)
    val optimized = optimizePath(toOptimize, fromPeer, toPeer)

    logger.trace(
      s"FROM PEER '${fromPeer.map(_.toString).getOrElse("None")}' TO PEER '${toPeer.map(_.toString).getOrElse("None")}'"
    )
    logger.trace("                     Optimized: " + optimized)

    optimized
  }

  /**
   * Removes cycles from the path
   *
   * @param peerIds
   *   peers to walk trough
   * @param fromPeer
   *   getting from the previous peer
   * @param toPeer
   *   getting to the next peer
   * @return
   *   optimal path with no duplicates
   */
  private def optimizePath(
    peerIds: Chain[ValueModel],
    fromPeer: Option[ValueModel],
    toPeer: Option[ValueModel]
  ): Chain[ValueModel] = {
    val optimized = peerIds.foldLeft(Chain.empty[ValueModel]) {
      case (acc, p) if acc.lastOption.contains(p) => acc
      case (acc, p) if acc.contains(p) => acc.takeWhile(_ != p) :+ p
      case (acc, p) => acc :+ p
    }

    logger.trace(s"PEER IDS: $optimized")
    logger.trace(s"FROM PEER: $fromPeer")
    logger.trace(s"TO PEER: $toPeer")

    val skipFrom = optimized.uncons match {
      case Some((head, tail)) if fromPeer.contains(head) => tail
      case _ => optimized
    }

    val skipTo = skipFrom.initLast match {
      case Some((init, last)) if toPeer.contains(last) => init
      case _ => skipFrom
    }

    skipTo
  }

  @tailrec
  private def skipCommonPrefix[T](chain1: Chain[T], chain2: Chain[T]): (Chain[T], Chain[T]) =
    (chain1, chain2) match {
      case (c ==: ctail, p ==: ptail) if c == p => skipCommonPrefix(ctail, ptail)
      case _ => chain1 -> chain2
    }
}
