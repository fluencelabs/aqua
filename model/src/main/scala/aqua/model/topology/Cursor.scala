package aqua.model.topology

import Topology.Tree
import aqua.model.func.body.{OnTag, OpTag, ParTag, SeqTag}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree

case class Cursor(point: ChainZipper[Tree], loc: Location) {

  def downLoc(tree: Tree): Location =
    loc.down(point.copy(current = tree))

  def prevOnTags: Chain[OnTag] =
    Chain
      .fromSeq(
        point.prev.lastOption
          .orElse(loc.lastLeftSeq.map(_._1.current))
          .toList
          .flatMap(Cursor.rightBoundary)
          .takeWhile {
            case ParTag => false
            case _ => true
          }
      )
      .collect { case o: OnTag =>
        o
      }

  def nextOnTags: Chain[OnTag] =
    Chain
      .fromSeq(
        loc.lastRightSeq
          .map(_._1.current)
          .toList
          .flatMap(Cursor.leftBoundary)
          .takeWhile {
            case ParTag => false
            case _ => true
          }
      )
      .collect { case o: OnTag =>
        o
      }
}

object Cursor {

  def rightBoundary(root: Tree): LazyList[OpTag] =
    root.head #:: LazyList.unfold(root.tail)(_.value.lastOption.map(lo => lo.head -> lo.tail))

  def leftBoundary(root: Tree): LazyList[OpTag] =
    root.head #:: LazyList.unfold(root.tail)(_.value.headOption.map(lo => lo.head -> lo.tail))

  def transform(root: Tree)(f: Cursor => List[Tree]): Option[Tree] = {
    def step(cursor: Cursor): Option[Tree] =
      f(cursor) match {
        case Nil => None
        case h :: Nil =>
          val np = cursor.downLoc(h)
          Some(h.copy(tail = h.tail.map(ChainZipper.fromChainMap(_)(cz => step(Cursor(cz, np))))))
        case hs =>
          ChainZipper
            .fromChain(Chain.fromSeq(hs))
            .map(cursor.point.replaceInjecting)
            .map { cfh =>
              val np = cursor.loc.down(cfh)
              cfh.current.copy(tail =
                cfh.current.tail.map(ChainZipper.fromChainMap(_)(cz => step(Cursor(cz, np))))
              )
            }
            .uncons
            .map {
              case (h, t) if t.isEmpty => h
              case (h, t) => Cofree(SeqTag, Eval.later(h +: t))
            }
      }
    step(Cursor(ChainZipper.one(root), Location()))
  }

}
