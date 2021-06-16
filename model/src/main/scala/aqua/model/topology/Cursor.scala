package aqua.model.topology

import Topology.Tree
import aqua.model.func.body.{OnTag, OpTag, ParTag, SeqTag}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import cats.syntax.functor._

case class Cursor(point: ChainZipper[Tree], loc: Location) {

  def downLoc(tree: Tree): Location =
    loc.down(point.copy(current = tree))

  def mapParent(f: Tree => Tree): Cursor =
    copy(loc =
      Location(
        loc.path match {
          case parent :: tail => parent.copy(current = f(parent.current)) :: tail
          case path => path
        }
      )
    )

  def prevOnTags: Chain[OnTag] =
    Chain
      .fromOption(
        point.prev.lastOption
          .map(t => loc.pathOn -> t)
          .orElse(loc.lastLeftSeq.map(_.map(_.pathOn).swap.map(_.current)))
      )
      .flatMap(pt => pt._1.widen[OpTag] ++ Cursor.rightBoundary(pt._2))
      .takeWhile {
        case ParTag => false
        case _ => true
      }
      .collect { case o: OnTag =>
        o
      }

  def nextOnTags: Chain[OnTag] =
    Chain
      .fromOption(
        loc.lastRightSeq
          .map(_.map(_.pathOn).swap.map(_.current))
      )
      .flatMap(pt => pt._1 ++ Cursor.leftBoundary(pt._2))
      .takeWhile {
        case ParTag => false
        case _ => true
      }
      .collect { case o: OnTag =>
        o
      }
}

object Cursor {

  def rightBoundary(root: Tree): Chain[OpTag] =
    Chain
      .fromSeq(
        root.head #:: LazyList.unfold(root.tail)(_.value.lastOption.map(lo => lo.head -> lo.tail))
      )

  def leftBoundary(root: Tree): Chain[OpTag] =
    Chain
      .fromSeq(
        root.head #:: LazyList.unfold(root.tail)(_.value.headOption.map(lo => lo.head -> lo.tail))
      )

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
