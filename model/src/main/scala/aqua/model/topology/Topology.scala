package aqua.model.topology

import aqua.model.func.body._
import aqua.model.topology.ChainZipper.Matchers._
import aqua.model.topology.Location.Matchers._
import aqua.model.{ValueModel, VarModel}
import aqua.types.{BoxType, ScalarType}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import wvlet.log.LogSupport

import scala.annotation.tailrec

object Topology extends LogSupport {
  type Tree = Cofree[Chain, OpTag]

  def resolve(op: Tree): Tree =
    Cofree
      .cata[Chain, OpTag, Tree](resolveOnMoves(op)) {
        case (SeqTag | _: OnTag, children) =>
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
            `current`(currentTree),
            loc @ `head`(parent: GroupTag) /: _
          ) =>
        val (specified, peerId) = specifyWay(currentTree.head, loc)
        val peerIdC = Chain.fromOption(peerId)
        val currentSpecifiedTree = currentTree.copy(head = specified)

        val getThere = (currentSpecifiedTree.head, loc.pathOn) match {
          case (OnTag(pid, _), h :: _) if h.peerId == pid => Chain.empty[ValueModel]
          case (OnTag(_, via), h :: _) =>
            h.via.reverse ++ via
          case (_, _) => Chain.empty[ValueModel]
        }

        val prevOn = c.prevOnTags

        // full paths from previous `on` tags
        val prevPath = prevOn.map { case OnTag(c, v) =>
          v.reverse :+ c
        }

        val prevPathFlat = prevPath.flatMap(identity)

        val nextOn = parent match {
          case ParTag | XorTag => c.nextOnTags
          case _ => Chain.empty[OnTag]
        }
        val nextPath = (if (nextOn.nonEmpty) getThere.reverse else Chain.empty) ++ nextOn.map {
          case OnTag(_, v) =>
            v.reverse
        }
          .flatMap(identity) ++ Chain.fromOption(
          // Dirty fix for join behaviour
          nextOn.lastOption.filter(_ => parent == ParTag).map(_.peerId)
        )

        if (prevOn.isEmpty && getThere.isEmpty) {
          currentSpecifiedTree :: Nil
        } else {
          val wayBeforeFull = prevPathFlat ++ loc.pathViaChain ++ getThere ++ peerIdC

          // filter optimized path by previous call peerId and current call peerId
          // because on optimization they will stay on their's first and last positions
          val optimizedWayBefore = optimizePath(wayBeforeFull).filter { vm =>
            val containsTarget = peerIdC.contains(vm)
            val containsPrevTargets = prevPath.find(_.lastOption.contains(vm)).isDefined
            !(containsTarget || containsPrevTargets)
          }
          val wayBefore = through(optimizedWayBefore)

          val wayAfterFull = peerIdC ++ nextPath

          // filter optimized path by current call peerId
          // because on optimization it will stay on their first position
          val optimizedWayAfter = optimizePath(wayAfterFull).filter(vm => !peerIdC.contains(vm))
          val wayAfter = through(optimizedWayAfter, reversed = true)

          val fullWay = (wayBefore.append(currentSpecifiedTree) ++ wayAfter).toList
          debug(s"fullWay to ${peerId}: ${fullWay.map(_.forceAll)}")
          fullWay
        }

      case Cursor(ChainZipper(_, cf, _), loc) =>
        val (specified, _) = specifyWay(cf.head, loc)
        cf.copy(head = specified) :: Nil
    }

  // specify peerId for CallServiceTag depends on location, return this peerId
  def specifyWay(tag: OpTag, loc: Location): (OpTag, Option[ValueModel]) = tag match {
    case c: CallServiceTag if c.peerId.isEmpty =>
      val lastOn = loc.lastOn.map(_.peerId)
      (c.copy(peerId = lastOn), lastOn)
    case t => (t, None)
  }

  // If same IDs are found in a row, does noop only once
  // if there's a chain like a -> b -> c -> ... -> b -> g, remove everything between b and b
  def optimizePath(peerIds: Chain[ValueModel]): Chain[ValueModel] =
    peerIds
      .foldLeft(Chain.empty[ValueModel]) {
        case (acc, p) if acc.lastOption.contains(p) => acc
        case (acc, p) if acc.contains(p) => acc.takeWhile(_ != p) :+ p
        case (acc, p) => acc :+ p
      }

  // Walks through peer IDs, doing a noop function on each
  def through(peerIds: Chain[ValueModel], reversed: Boolean = false): Chain[Tree] = {
    debug("through peerIds: " + peerIds)
    peerIds.map { v =>
      v.lastType match {
        case _: BoxType =>
          val itemName = "-via-peer-"

          val noop = FuncOps.noop(VarModel(itemName, ScalarType.string))
          val next = FuncOps.next(itemName)
          val seq =
            if (reversed)
              FuncOps.seq(
                next,
                noop
              )
            else
              FuncOps.seq(
                noop,
                next
              )

          FuncOps.meta(
            FuncOps.fold(
              itemName,
              v,
              seq
            ),
            skipTopology = true
          )
        case _ =>
          FuncOps.noop(v)
      }
    }
      .map(_.tree)
  }

}
