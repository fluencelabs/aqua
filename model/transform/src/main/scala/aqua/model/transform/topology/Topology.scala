package aqua.model.transform.topology

import aqua.model.transform.cursor.ChainZipper
import aqua.model.func.raw.*
import aqua.model.transform.res.*
import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.types.{BoxType, ScalarType}
import cats.Eval
import cats.data.Chain.nil
import cats.data.{Chain, NonEmptyChain, NonEmptyList, OptionT}
import cats.free.Cofree
import cats.syntax.traverse.*
import scribe.Logging

case class Topology(rc: RawCursor) {

  def isOnTag: Boolean =
    rc.tag match {
      case _: OnTag => true
      case _ => false
    }

  def isForTag: Boolean =
    rc.tag match {
      case _: ForTag => true
      case _ => false
    }

  def isSeqGroupTag: Boolean =
    rc.tag match {
      case _: SeqGroupTag => true
      case _ => false
    }

  lazy val pathOn: List[OnTag] = rc.tagsPath.collect { case o: OnTag =>
    o
  }

  def isParentSeqGroupTag: Boolean =
    rc.moveUp.exists(_.topology.isSeqGroupTag)

  // Before the left boundary of this element, what was the scope
  def beforeOn: List[OnTag] =
    // If we're inside seq, see where prev tag ends
    rc.toPrevSibling
      .filter(_.topology.isParentSeqGroupTag)
      .map(_.topology.afterOn) orElse
      // Otherwise go to the parent, see where it begins
      rc.moveUp.map(_.topology.beginsOn) getOrElse
      // This means, we have no parent; then we're where we should be
      beginsOn

  // Inside the left boundary of this element, what should be the scope
  def beginsOn: List[OnTag] =
    // That's just where we must be
    rc.pathOn

  // After this element is done, what is the scope
  def endsOn: List[OnTag] =
    rc.toLastChild
      .map(_.topology)
      .filter(_.isParentSeqGroupTag)
      .map(_.afterOn) getOrElse beginsOn

  // After this element is done, where should it move to prepare for the next one
  def afterOn: List[OnTag] = endsOn

  def pathBefore: Chain[ValueModel] = PathFinder.findPath(
    Chain.fromSeq(beforeOn),
    Chain.fromSeq(beginsOn),
    beforeOn.headOption.map(_.peerId),
    beginsOn.headOption.map(_.peerId)
  )

  def pathAfter: Chain[ValueModel] = PathFinder.findPath(
    Chain.fromSeq(endsOn),
    Chain.fromSeq(afterOn),
    endsOn.headOption.map(_.peerId),
    afterOn.headOption.map(_.peerId)
  )
}

object Topology extends Logging {
  type Tree = Cofree[Chain, RawTag]
  type Res = Cofree[Chain, ResolvedOp]

  def resolve(op: Tree): Res = {
    val resolved = resolveOnMoves(op).value
    Cofree
      .cata[Chain, ResolvedOp, Res](resolved) {
        case (SeqRes, children) =>
          Eval.later {
            children.uncons
              .filter(_._2.isEmpty)
              .map(_._1)
              .getOrElse(
                Cofree(
                  SeqRes,
                  Eval.now(children.flatMap {
                    case Cofree(SeqRes, ch) => ch.value
                    case cf => Chain.one(cf)
                  })
                )
              )
          }
        case (head, children) => Eval.later(Cofree(head, Eval.now(children)))
      }
      .value
  }

  def wrap(cz: ChainZipper[Res]): Chain[Res] =
    Chain.one(
      if (cz.prev.nonEmpty || cz.next.nonEmpty) Cofree(SeqRes, Eval.now(cz.chain))
      else cz.current
    )

  def resolveOnMoves(op: Tree): Eval[Res] = {
    val cursor = RawCursor(NonEmptyList.one(ChainZipper.one(op)), None)
    // TODO: remove var
    var i = 0
    def nextI = {
      i = i + 1
      i
    }

    val resolvedCofree = cursor
      .cata(wrap) { rc =>
        logger.debug(s"<:> $rc")
        val resolved =
          MakeRes
            .resolve(rc.currentPeerId, nextI)
            .lift
            .apply(rc.tag)

        logger.trace("Resolved: " + resolved)

        val chainZipperEv = resolved.traverse(cofree =>
          Eval.later {
            val cz = ChainZipper(
              through(rc.topology.pathBefore),
              cofree,
              through(rc.topology.pathAfter)
            )
            if (cz.next.nonEmpty || cz.prev.nonEmpty) {
              logger.debug(s"Resolved   $rc -> $cofree")
              if (cz.prev.nonEmpty)
                logger.trace("From prev: " + cz.prev.map(_.head).toList.mkString(" -> "))
              if (cz.next.nonEmpty)
                logger.trace("To next:   " + cz.next.map(_.head).toList.mkString(" -> "))
            } else logger.debug(s"EMPTY    $rc -> $cofree")
            cz
          }
        )

        OptionT[Eval, ChainZipper[Res]](chainZipperEv)
      }

    logger.trace("Resolved Cofree: " + resolvedCofree.value.map(_.forceAll))

    resolvedCofree.map(NonEmptyChain.fromChain(_).map(_.uncons)).map {
      case None =>
        logger.error("Topology emitted nothing")
        Cofree(SeqRes, MakeRes.nilTail)
      case Some((el, `nil`)) => el
      case Some((el, tail)) =>
        logger.warn("Topology emitted many nodes, that's unusual")
        Cofree(SeqRes, Eval.now(el +: tail))
    }
  }

  // Walks through peer IDs, doing a noop function on each
  // If same IDs are found in a row, does noop only once
  // if there's a chain like a -> b -> c -> ... -> b -> g, remove everything between b and b
  def through(peerIds: Chain[ValueModel], reversed: Boolean = false): Chain[Res] =
    peerIds.map { v =>
      v.lastType match {
        case _: BoxType =>
          val itemName = "-via-peer-"

          MakeRes.fold(
            itemName,
            v,
            if (reversed)
              MakeRes.seq(
                MakeRes.next(itemName),
                MakeRes.noop(VarModel(itemName, ScalarType.string))
              )
            else
              MakeRes.seq(
                MakeRes.noop(VarModel(itemName, ScalarType.string)),
                MakeRes.next(itemName)
              )
          )
        case _ =>
          MakeRes.noop(v)
      }
    }
}
