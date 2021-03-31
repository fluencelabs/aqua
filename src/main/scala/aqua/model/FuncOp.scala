package aqua.model

import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import cats.kernel.Semigroup
import cats.syntax.apply._
import cats.syntax.functor._

case class FuncOp(tree: Cofree[Chain, OpTag]) extends Model {
  def head: OpTag = tree.head
  def isRightAssoc: Boolean = head == XorTag || head == ParTag

  def cata[T](folder: (OpTag, Chain[T]) => Eval[T]): Eval[T] =
    Cofree.cata(tree)(folder)

  def definesValueNames: Eval[Set[String]] = cata[Set[String]] {
    case (CallArrowTag(_, _, Call(_, Some(export))), acc) =>
      Eval.later(acc.foldLeft(Set(export))(_ ++ _))
    case (CallServiceTag(_, _, Call(_, Some(export)), _), acc) =>
      Eval.later(acc.foldLeft(Set(export))(_ ++ _))
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
          case c: CallArrowTag => c.copy(call = c.call.mapExport(n => vals.getOrElse(n, n)))
          case c: CallServiceTag => c.copy(call = c.call.mapExport(n => vals.getOrElse(n, n)))
          case t: ForTag if vals.contains(t.item) => t.copy(item = vals(t.item))
          case t: NextTag if vals.contains(t.item) => t.copy(item = vals(t.item))
          case t => t
        }
      )
    )

  def resolveTopology(): FuncOp =
    FuncOp(FuncOp.transformWithPath(tree) {
      case (path, c: CallServiceTag) =>
        Cofree[Chain, OpTag](
          c.copy(peerId = path.collectFirst { case OnTag(peerId, _) =>
            peerId
          }),
          Eval.now(Chain.empty)
        )
      case (path, OnTag(pid, via)) if via.nonEmpty =>
        Cofree[Chain, OpTag](
          OnTag(pid, Nil),
          Eval.now(
            Chain.fromSeq(
              via
                .map(p =>
                  FuncOp.wrap(
                    OnTag(p, Nil),
                    FuncOp.leaf(
                      CallServiceTag(LiteralModel("\"op\""), "identity", Call(Nil, None), Some(p))
                    )
                  )
                )
                .map(_.tree)
            )
          )
        )
      case (_, t) =>
        Cofree[Chain, OpTag](t, Eval.now(Chain.empty))
    })

  def :+:(prev: FuncOp): FuncOp =
    FuncOp.RightAssocSemi.combine(prev, this)
}

object FuncOp {

  def traverseA[A](cf: Cofree[Chain, OpTag], init: A)(
    f: (A, OpTag) => (A, Cofree[Chain, OpTag])
  ): Eval[(A, Cofree[Chain, OpTag])] = {
    val (headA, head) = f(init, cf.head)
    // TODO: it should be in standard library, with some other types
    cf.tail
      .map(_.foldLeft[(A, Chain[Cofree[Chain, OpTag]])]((headA, head.tailForced)) {
        case ((aggrA, aggrTail), child) =>
          traverseA(child, aggrA)(f).value.map(aggrTail.append)
      })
      .map(_.map(ch => head.copy(tail = Eval.now(ch))))
  }

  def transformWithPath(cf: Cofree[Chain, OpTag], path: List[OpTag] = Nil)(
    f: (List[OpTag], OpTag) => Cofree[Chain, OpTag]
  ): Cofree[Chain, OpTag] = {
    val h = f(path, cf.head)
    Cofree[Chain, OpTag](
      h.head,
      (h.tail, cf.tail)
        .mapN(_ ++ _)
        // IF make foldLeft here, will be possible to get info from prev sibling
        .map(_.map(transformWithPath(_, h.head :: path)(f)))
    )
  }

  object RightAssocSemi extends Semigroup[FuncOp] {

    override def combine(x: FuncOp, y: FuncOp): FuncOp = (x.tree.head, y.tree.head) match {
      case (ParTag, ParTag) => FuncOp(y.tree.copy(tail = (x.tree.tail, y.tree.tail).mapN(_ ++ _)))
      case (XorTag, XorTag) => FuncOp(y.tree.copy(tail = (x.tree.tail, y.tree.tail).mapN(_ ++ _)))
      case (_, ParTag | XorTag) =>
        wrap(SeqTag, FuncOp(y.tree.copy(tail = y.tree.tail.map(_.prepend(x.tree)))))
      case _ => FuncOpSemigroup.combine(x, y)
    }
  }

  implicit object FuncOpSemigroup extends Semigroup[FuncOp] {

    override def combine(x: FuncOp, y: FuncOp): FuncOp = (x.tree.head, y.tree.head) match {
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
