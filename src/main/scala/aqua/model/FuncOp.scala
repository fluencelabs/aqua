package aqua.model

import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import cats.kernel.Semigroup
import cats.syntax.apply._
import cats.syntax.functor._

case class FuncOp(tree: Cofree[Chain, OpTag]) extends Model {

  def cata[T](folder: (OpTag, Chain[T]) => Eval[T]): Eval[T] =
    Cofree.cata(tree)(folder)

  def definesValueNames: Eval[Set[String]] = cata[Set[String]] {
    case (CoalgebraTag(_, _, Call(_, Some(export))), acc) => Eval.later(acc.foldLeft(Set(export))(_ ++ _))
    case (CallServiceTag(_, _, Call(_, Some(export))), acc) => Eval.later(acc.foldLeft(Set(export))(_ ++ _))
    case (NextTag(export), acc) => Eval.later(acc.foldLeft(Set(export))(_ ++ _))
    case (_, acc) => Eval.later(acc.foldLeft(Set.empty[String])(_ ++ _))
  }

  def resolveValues(vals: Map[String, ValueModel]): FuncOp = FuncOp(tree.map[OpTag](_.mapValues {
    case v: VarModel =>
      vals.get(v.name) match {
        case Some(vv: VarModel) => v.deriveFrom(vv)
        case Some(vv) => vv // TODO check that lambda is empty, otherwise error
        case None => v // Should not happen
      }
    case v => v
  }))

}

object FuncOp {

  def traverseA[A](cf: Cofree[Chain, OpTag], init: A)(
    f: (A, OpTag) => (A, Cofree[Chain, OpTag])
  ): Eval[(A, Cofree[Chain, OpTag])] = {
    val (headA, head) = f(init, cf.head)
    cf.tail
      .map(_.foldLeft[(A, Chain[Cofree[Chain, OpTag]])]((headA, head.tailForced)) { case ((aggrA, aggrTail), child) =>
        traverseA(child, aggrA)(f).value.map(aggrTail.append)
      })
      .map(_.map(ch => head.copy(tail = Eval.now(ch))))
  }

  implicit object FuncOpSemigroup extends Semigroup[FuncOp] {

    override def combine(x: FuncOp, y: FuncOp): FuncOp = (x.tree.head, y.tree.head) match {
      case (_, ParTag | XorTag) => FuncOp(y.tree.copy(tail = (x.tree.tail, y.tree.tail).mapN(_ ++ _)))
      case (SeqTag, SeqTag) => FuncOp(y.tree.copy(tail = (x.tree.tail, y.tree.tail).mapN(_ ++ _)))
      case (_, SeqTag) => FuncOp(y.tree.copy(tail = y.tree.tail.map(_.prepend(x.tree))))
      case (SeqTag, _) => FuncOp(x.tree.copy(tail = x.tree.tail.map(_.append(y.tree))))
      case _ => node(SeqTag, Chain(x, y))
    }
  }

  def leaf(tag: OpTag): FuncOp = FuncOp(Cofree[Chain, OpTag](tag, Eval.now(Chain.empty)))

  def wrap(tag: OpTag, child: FuncOp): FuncOp = node(tag, Chain.one(child))

  def node(tag: OpTag, children: Chain[FuncOp]): FuncOp =
    FuncOp(Cofree[Chain, OpTag](tag, Eval.later(children.map(_.tree))))
}
