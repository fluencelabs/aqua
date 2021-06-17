package aqua.model.topology

import aqua.model.{ValueModel, VarModel}
import aqua.model.func.body._
import cats.Eval
import cats.data.Chain
import cats.data.Chain.{:==, ==:, nil}
import cats.free.Cofree
import ChainZipper.Matchers._
import Location.Matchers._
import aqua.types.{BoxType, ScalarType}
import wvlet.log.LogSupport

import scala.annotation.tailrec

object Topology extends LogSupport {
  type Tree = Cofree[Chain, OpTag]

  def resolve(op: Tree): Tree =
    Cofree
      .cata[Chain, OpTag, Tree](resolveOnMoves(op)) {
        case (SeqTag | _: OnTag | MetaTag(false, _, SeqTag | _: OnTag), children) =>
          Eval.later(
            Cofree(
              SeqTag,
              Eval.now(children.flatMap {
                case Cofree(SeqTag, ch) => ch.value
                case cf => Chain.one(cf)
              })
            )
          )
        case (head, children) => Eval.later(Cofree(head, Eval.now(children)))
      }
      .value

  def resolveOnMoves(op: Tree): Tree =
    Cursor
      .transform(op)(transformWalker)
      .getOrElse(op)

  @tailrec
  private def transformWalker(c: Cursor): List[Tree] =
    c match {
      case Cursor(_, `head`(parent: MetaTag) /: _) if !parent.skipTopology =>
        transformWalker(c.mapParent(p => p.copy(parent.op, p.tail)))

      case Cursor(
            `current`(cf),
            loc @ `head`(parent: GroupTag) /: _
          ) =>
        // Set the service call IDs
        val cfu = cf.copy(setServiceCallPeerId(cf.head, loc))

        // We need to get there, finally
        val currentPeerId = Chain.fromOption(loc.lastOn.map(_.peerId))

        debug("Going to handle: " + cf.head)

        val fromPrevToCurrentPath = fromPrevToCurrent(c, currentPeerId)
        if (fromPrevToCurrentPath.nonEmpty) debug("BEFORE = " + fromPrevToCurrentPath)

        val fromCurrentToNextPath = fromCurrentToNext(parent, c, currentPeerId)
        if (fromCurrentToNextPath.nonEmpty) debug("NEXT = " + fromCurrentToNextPath)

        (through(fromPrevToCurrentPath)
          .append(cfu) ++ through(fromCurrentToNextPath, reversed = true)).toList

      case Cursor(ChainZipper(_, cf, _), loc) =>
        cf.copy(setServiceCallPeerId(cf.head, loc)) :: Nil
    }

  def fromPrevToCurrent(c: Cursor, currentPeerId: Chain[ValueModel]): Chain[ValueModel] = {
    val prevOn = c.prevOnTags
    val currentOn = c.loc.pathOn

    val wasHandled = c.pathToRoot.collectFirst { case cc @ Cursor(_, `head`(_: GroupTag) /: _) =>
      cc.loc.pathOn
    }.exists(cclp =>
      cclp == currentOn && {
        val (c1, _) = skipCommonPrefix(prevOn, cclp)
        c1.isEmpty
      }
    )

    // Need to get from there
    val prevPeerId =
      Chain.fromOption(prevOn.lastOption.map(_.peerId) orElse c.loc.firstOn.map(_.peerId))

    if (wasHandled) Chain.empty[ValueModel]
    else
      findPath(prevOn, currentOn, prevPeerId, currentPeerId)
  }

  def fromCurrentToNext(
    parent: OpTag,
    c: Cursor,
    currentPeerId: Chain[ValueModel]
  ): Chain[ValueModel] = {
    // Usually we don't need to go next
    val nextOn = parent match {
      case ParTag =>
        val exports = FuncOp(c.point.current).exportsVarNames.value
        if (exports.isEmpty) Chain.empty[OnTag]
        else {
          val isUsed = c.pathToRoot.tail.collect {
            case Cursor(cz, `head`(gt: GroupTag) /: _) if gt != ParTag =>
              cz.next.map(FuncOp(_)).map(_.usesVarNames)
          }.exists(_.exists(_.value.intersect(exports).nonEmpty))
          if (isUsed) c.nextOnTags else Chain.empty[OnTag]
        }
      case XorTag if c.point.prev.nonEmpty => c.nextOnTags
      case _ => Chain.empty[OnTag]
    }
    val nextPeerId =
      if (nextOn.nonEmpty) Chain.fromOption(nextOn.lastOption.map(_.peerId)) else currentPeerId

    val targetOn: Option[OnTag] = c.point.current.head match {
      case o: OnTag => Option(o)
      case _ => None
    }
    val currentOn = c.loc.pathOn
    val currentOnInside = targetOn.fold(currentOn)(currentOn :+ _)
    findPath(
      currentOnInside,
      nextOn,
      currentPeerId,
      nextPeerId
    )
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

    debug("FIND PATH " + fromFix)
    debug("       -> " + toFix)
    debug("                     Optimized: " + optimized)
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

  // Walks through peer IDs, doing a noop function on each
  // If same IDs are found in a row, does noop only once
  // if there's a chain like a -> b -> c -> ... -> b -> g, remove everything between b and b
  def through(peerIds: Chain[ValueModel], reversed: Boolean = false): Chain[Tree] =
    peerIds.map { v =>
      v.lastType match {
        case _: BoxType =>
          val itemName = "-via-peer-"

          FuncOps.meta(
            FuncOps.fold(
              itemName,
              v,
              if (reversed)
                FuncOps.seq(
                  FuncOps.next(itemName),
                  FuncOps.noop(VarModel(itemName, ScalarType.string))
                )
              else
                FuncOps.seq(
                  FuncOps.noop(VarModel(itemName, ScalarType.string)),
                  FuncOps.next(itemName)
                )
            ),
            skipTopology = true
          )
        case _ =>
          FuncOps.noop(v)
      }
    }
      .map(_.tree)

  def setServiceCallPeerId(tag: OpTag, loc: Location): OpTag = tag match {
    case c: CallServiceTag if c.peerId.isEmpty =>
      c.copy(peerId = loc.lastOn.map(_.peerId))
    case t => t
  }
}
