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
    case (CallServiceTag(_, _, Call(_, Some(export)), _), acc) => Eval.later(acc.foldLeft(Set(export))(_ ++ _))
    case (NextTag(export), acc) => Eval.later(acc.foldLeft(Set(export))(_ ++ _))
    case (_, acc) => Eval.later(acc.foldLeft(Set.empty[String])(_ ++ _))
  }

  def resolveValues(vals: Map[String, ValueModel]): FuncOp =
    FuncOp(tree.map[OpTag](_.mapValues(_.resolveWith(vals))))

  def rename(vals: Map[String, String]): FuncOp =
    FuncOp(
      tree.map[OpTag](op =>
        op.mapValues {
          case v: VarModel if vals.contains(v.name) => v.copy(name = vals(v.name))
          case v => v
        } match {
          case c: CoalgebraTag => c.copy(call = c.call.mapExport(n => vals.getOrElse(n, n)))
          case c: CallServiceTag => c.copy(call = c.call.mapExport(n => vals.getOrElse(n, n)))
          case t: ForTag if vals.contains(t.item) => t.copy(item = vals(t.item))
          case t: NextTag if vals.contains(t.item) => t.copy(item = vals(t.item))
          case t => t
        }
      )
    )

  def resolvePeerId: FuncOp =
    FuncOp(FuncOp.mapWithPath(tree) {
      case (path, c: CallServiceTag) =>
        Cofree[Chain, OpTag](
          c.copy(peerId = path.collectFirst { case OnTag(peerId) =>
            peerId
          }),
          Eval.now(Chain.empty)
        )
      case (_, t) => Cofree[Chain, OpTag](t, Eval.now(Chain.empty))
    })

}

object FuncOp {

  def traverseA[A](cf: Cofree[Chain, OpTag], init: A)(
    f: (A, OpTag) => (A, Cofree[Chain, OpTag])
  ): Eval[(A, Cofree[Chain, OpTag])] = {
    val (headA, head) = f(init, cf.head)
    // TODO: it should be in standard library, with some other types
    cf.tail
      .map(_.foldLeft[(A, Chain[Cofree[Chain, OpTag]])]((headA, head.tailForced)) { case ((aggrA, aggrTail), child) =>
        traverseA(child, aggrA)(f).value.map(aggrTail.append)
      })
      .map(_.map(ch => head.copy(tail = Eval.now(ch))))
  }

  def mapWithPath(cf: Cofree[Chain, OpTag], path: List[OpTag] = Nil)(
    f: (List[OpTag], OpTag) => Cofree[Chain, OpTag]
  ): Cofree[Chain, OpTag] = {
    val h = f(path, cf.head)
    Cofree[Chain, OpTag](h.head, (h.tail, cf.tail).mapN(_ ++ _).map(_.map(mapWithPath(_, h.head :: path)(f))))
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
