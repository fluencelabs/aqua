package aqua.model.topology

import aqua.model.{ValueModel, VarModel}
import aqua.model.func.body._
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import ChainZipper.Matchers._
import Location.Matchers._
import aqua.types.{BoxType, ScalarType}

import scala.annotation.tailrec

object Topology {
  type Tree = Cofree[Chain, OpTag]

  // Walks through peer IDs, doing a noop function on each
  // If same IDs are found in a row, does noop only once
  // if there's a chain like a -> b -> c -> ... -> b -> g, remove everything between b and b
  def through(peerIds: Chain[ValueModel], reversed: Boolean = false): Chain[Tree] =
    peerIds
      .foldLeft(Chain.empty[ValueModel]) {
        case (acc, p) if acc.lastOption.contains(p) => acc
        case (acc, p) if acc.contains(p) => acc.takeWhile(_ != p) :+ p
        case (acc, p) => acc :+ p
      }
      .map { v =>
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

  def mapTag(tag: OpTag, loc: Location): OpTag = tag match {
    case c: CallServiceTag if c.peerId.isEmpty =>
      c.copy(peerId = loc.lastOn.map(_.peerId))
    case t => t
  }

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

  @tailrec
  private def transformWalker(c: Cursor): List[Tree] =
    c match {
      case Cursor(_, `head`(parent: MetaTag) /: _) if !parent.skipTopology =>
        transformWalker(c.mapParent(p => p.copy(parent.op, p.tail)))

      case Cursor(
            `current`(cf),
            loc @ `head`(parent: GroupTag) /: _
          ) =>
        val cfu = cf.copy(mapTag(cf.head, loc))

        val getThere = (cfu.head, loc.pathOn) match {
          case (OnTag(pid, _), h :: _) if h.peerId == pid => Chain.empty[ValueModel]
          case (OnTag(_, via), h :: _) =>
            h.via.reverse ++ via
          case (_, _) => Chain.empty[ValueModel]
        }

        val prevOn = c.prevOnTags

        val prevPath = prevOn.map { case OnTag(_, v) =>
          v.reverse
        }
          .flatMap(identity)

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

        if (prevOn.isEmpty && getThere.isEmpty) cfu :: Nil
        else
          (through(prevPath ++ loc.pathViaChain ++ getThere)
            .append(cfu) ++ through(nextPath, reversed = true)).toList

      case Cursor(ChainZipper(_, cf, _), loc) =>
        cf.copy(mapTag(cf.head, loc)) :: Nil
    }

  def resolveOnMoves(op: Tree): Tree =
    Cursor
      .transform(op)(transformWalker)
      .getOrElse(op)

}
