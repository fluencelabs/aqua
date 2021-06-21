package aqua.model.func.raw

import aqua.model.ValueModel
import cats.data.Chain
import cats.data.Chain.{:==, ==:, nil}
import wvlet.log.LogSupport

import scala.annotation.tailrec

object PathFinder extends LogSupport {

  def find(from: RawCursor, to: RawCursor): Chain[ValueModel] = {

    val fromOn = Chain.fromSeq(from.pathOn).reverse
    val toOn = Chain.fromSeq(to.pathOn).reverse

    val wasHandled = to.pathToRoot.collectFirst {
      case c if c.parentTag.exists(_.isInstanceOf[GroupTag]) =>
        c.pathOn
    }.exists(cclp =>
      cclp == to.pathOn && {
        val (c1, _) = skipCommonPrefix(fromOn, Chain.fromSeq(cclp).reverse)
        c1.isEmpty
      }
    )

    if (wasHandled) {
      warn("Was handled")
      info(" :: " + from)
      info(" -> " + to)
      Chain.empty
    } else {

      warn("Find path")
      info(" :: " + from)
      info(" -> " + to)
      findPath(
        fromOn,
        toOn,
        Chain.fromOption(from.currentPeerId),
        Chain.fromOption(to.currentPeerId)
      )
    }
  }

  def optimizePath(
    peerIds: Chain[ValueModel],
    prefix: Chain[ValueModel],
    suffix: Chain[ValueModel]
  ): Chain[ValueModel] = {
    val optimized = peerIds
      .foldLeft(Chain.empty[ValueModel]) {
        case (acc, p) if acc.lastOption.contains(p) => acc
        case (acc, p) if acc.contains(p) => acc.takeWhile(_ != p) :+ p
        case (acc, p) => acc :+ p
      }
    val noPrefix = skipPrefix(optimized, prefix, optimized)
    skipSuffix(noPrefix, suffix, noPrefix)
  }

  def findPath(
    fromOn: Chain[OnTag],
    toOn: Chain[OnTag],
    fromPeer: Chain[ValueModel],
    toPeer: Chain[ValueModel]
  ): Chain[ValueModel] = {
    val (from, to) = skipCommonPrefix(fromOn, toOn)
    val fromFix =
      if (from.isEmpty && fromPeer != toPeer) Chain.fromOption(fromOn.lastOption) else from
    val toFix = if (to.isEmpty && fromPeer != toPeer) Chain.fromOption(toOn.lastOption) else to
    val fromTo = fromFix.reverse.flatMap(_.via.reverse) ++ toFix.flatMap(_.via)
    val optimized = optimizePath(fromPeer ++ fromTo ++ toPeer, fromPeer, toPeer)

    info("FIND PATH " + fromFix)
    info("       -> " + toFix)
    info(s"$fromPeer $toPeer")
    info("                     Optimized: " + optimized)
    optimized
  }

  @tailrec
  def skipPrefix[T](chain: Chain[T], prefix: Chain[T], init: Chain[T]): Chain[T] =
    (chain, prefix) match {
      case (c ==: ctail, p ==: ptail) if c == p => skipPrefix(ctail, ptail, init)
      case (_, `nil`) => chain
      case (_, _) => init
    }

  @tailrec
  def skipCommonPrefix[T](chain1: Chain[T], chain2: Chain[T]): (Chain[T], Chain[T]) =
    (chain1, chain2) match {
      case (c ==: ctail, p ==: ptail) if c == p => skipCommonPrefix(ctail, ptail)
      case _ => chain1 -> chain2
    }

  @tailrec
  def skipSuffix[T](chain: Chain[T], suffix: Chain[T], init: Chain[T]): Chain[T] =
    (chain, suffix) match {
      case (cinit :== c, pinit :== p) if c == p => skipSuffix(cinit, pinit, init)
      case (_, `nil`) => chain
      case (_, _) => init
    }
}
