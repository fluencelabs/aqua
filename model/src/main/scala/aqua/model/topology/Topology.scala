package aqua.model.topology

import aqua.model.ValueModel
import aqua.model.func.body._
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import ChainZipper.Matchers._
import Location.Matchers._

object Topology {
  type Tree = Cofree[Chain, OpTag]

  def rightBoundary(root: Tree): LazyList[OpTag] =
    root.head #:: LazyList.unfold(root.tail)(_.value.lastOption.map(lo => lo.head -> lo.tail))

  // Walks through peer IDs, doing a noop function on each
  // If same IDs are found in a row, does noop only once
  // if there's a chain like a -> b -> c -> ... -> b -> g, remove everything between b and b
  def through(peerIds: Chain[ValueModel]): Chain[Tree] =
    peerIds
      .foldLeft(Chain.empty[ValueModel]) {
        case (acc, p) if acc.lastOption.contains(p) => acc
        case (acc, p) if acc.contains(p) => acc.takeWhile(_ != p) :+ p
        case (acc, p) => acc :+ p
      }
      .map(FuncOps.noop)
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

  private def prevOnTags(cz: ChainZipper[Tree], loc: Location): Chain[OnTag] =
    Chain
      .fromSeq(
        cz.prev.lastOption
          .orElse(loc.lastLeftSeq.map(_._1.current))
          .toList
          .flatMap(rightBoundary)
          .takeWhile {
            case ParTag => false
            case _ => true
          }
      )
      .collect { case o: OnTag =>
        o
      }

  private def nextOnTags(loc: Location): Chain[OnTag] =
    Chain
      .fromSeq(
        loc.lastRightSeq
          .map(_._1.current)
          .toList
          .flatMap(rightBoundary)
          .takeWhile {
            case ParTag => false
            case _ => true
          }
      )
      .collect { case o: OnTag =>
        o
      }

  def resolveOnMoves(op: Tree): Tree =
    zipTransform(ChainZipper.one(op)) {
      case (
            cz @ `current`(cf),
            loc @ `head`(parent: GroupTag) /: _
          ) =>
        val cfu = cf.copy(mapTag(cf.head, loc))

        val getThere = (cfu.head, loc.pathOn) match {
          case (OnTag(pid, _), h :: _) if h.peerId == pid => Chain.empty[ValueModel]
          case (OnTag(_, via), h :: _) =>
            h.via.reverse ++ via
          case (_, _) => Chain.empty[ValueModel]
        }

        val prevOn = prevOnTags(cz, loc)

        val prevPath = prevOn.map { case OnTag(_, v) =>
          v.reverse
        }
          .flatMap(identity)

        val nextOn = parent match {
          case ParTag | XorTag => nextOnTags(loc)
          case _ => Chain.empty[OnTag]
        }
        val nextPath = nextOn.map { case OnTag(_, v) =>
          v.reverse
        }
          .flatMap(identity)

        if (prevOn.isEmpty && getThere.isEmpty) cfu :: Nil
        else
          (through(prevPath ++ loc.pathViaChain ++ getThere)
            .append(cfu) ++ through(nextPath)).toList

      case (ChainZipper(_, cf, _), loc) =>
        cf.copy(mapTag(cf.head, loc)) :: Nil
    }.getOrElse(op)

  def transformWithPath(cf: Tree, path: List[OpTag] = Nil)(
    f: (List[OpTag], Eval[Chain[Tree]]) => Tree
  ): Tree = {
    val newCf = f(cf.head :: path, cf.tail)
    Cofree[Chain, OpTag](
      newCf.head,
      newCf.tail.map(_.map(transformWithPath(_, newCf.head :: path)(f)))
    )
  }

  def zipTransform(cf: ChainZipper[Tree], location: Location = Location())(
    f: (ChainZipper[Tree], Location) => List[Tree]
  ): Option[Tree] = {
    f(cf, location) match {
      case Nil => None
      case h :: Nil =>
        val np = location down cf.copy(current = h)
        Some(h.copy(tail = h.tail.map(ChainZipper.fromChainMap(_)(zipTransform(_, np)(f)))))
      case hs =>
        ChainZipper
          .fromChain(Chain.fromSeq(hs))
          .map(cz => cz.copy(prev = cf.prev ++ cz.prev, next = cz.next ++ cf.next))
          .map { cfh =>
            val np = location.down(cfh)
            cfh.current.copy(tail =
              cfh.current.tail.map(ChainZipper.fromChainMap(_)(zipTransform(_, np)(f)))
            )
          }
          .uncons
          .map {
            case (h, t) if t.isEmpty => h
            case (h, t) => Cofree(SeqTag, Eval.later(h +: t))
          }
    }

  }
}
