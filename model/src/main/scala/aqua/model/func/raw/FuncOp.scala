package aqua.model.func.raw

import aqua.model.func.Call
import aqua.model.{Model, ValueModel, VarModel}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import cats.kernel.Semigroup
import cats.syntax.apply._
import cats.syntax.functor._

case class FuncOp(tree: Cofree[Chain, RawTag]) extends Model {
  def head: RawTag = tree.head

  lazy val isRightAssoc: Boolean = head match {
    case XorTag | ParTag => true
    case _: XorParTag => true
    case _ => false
  }

  def cata[T](folder: (RawTag, Chain[T]) => Eval[T]): Eval[T] =
    Cofree.cata(tree)(folder)

  def definesVarNames: Eval[Set[String]] = cata[Set[String]] {
    case (CallArrowTag(_, Call(_, Some(export))), acc) =>
      Eval.later(acc.foldLeft(Set(export.name))(_ ++ _))
    case (CallServiceTag(_, _, Call(_, Some(export))), acc) =>
      Eval.later(acc.foldLeft(Set(export.name))(_ ++ _))
    case (NextTag(export), acc) => Eval.later(acc.foldLeft(Set(export))(_ ++ _))
    case (_, acc) => Eval.later(acc.foldLeft(Set.empty[String])(_ ++ _))
  }

  def exportsVarNames: Eval[Set[String]] = cata[Set[String]] {
    case (CallArrowTag(_, Call(_, Some(export))), acc) =>
      Eval.later(acc.foldLeft(Set(export.name))(_ ++ _))
    case (CallServiceTag(_, _, Call(_, Some(export))), acc) =>
      Eval.later(acc.foldLeft(Set(export.name))(_ ++ _))
    case (_, acc) => Eval.later(acc.foldLeft(Set.empty[String])(_ ++ _))
  }

  // TODO: as it is used for checking of intersection, make it a lazy traverse with fail-fast
  def usesVarNames: Eval[Set[String]] = cata[Set[String]] {
    case (CallArrowTag(_, call), acc) =>
      Eval.later(acc.foldLeft(call.argVarNames)(_ ++ _))
    case (CallServiceTag(_, _, call), acc) =>
      Eval.later(acc.foldLeft(call.argVarNames)(_ ++ _))
    case (_, acc) => Eval.later(acc.foldLeft(Set.empty[String])(_ ++ _))
  }

  def resolveValues(vals: Map[String, ValueModel]): FuncOp =
    FuncOp(tree.map[RawTag](_.mapValues(_.resolveWith(vals))))

  def rename(vals: Map[String, String]): FuncOp =
    FuncOp(
      tree.map[RawTag](op =>
        op.mapValues {
          case v: VarModel if vals.contains(v.name) => v.copy(name = vals(v.name))
          case v => v
        } match {
          case c: CallArrowTag => c.copy(call = c.call.mapExport(n => vals.getOrElse(n, n)))
          case c: CallServiceTag => c.copy(call = c.call.mapExport(n => vals.getOrElse(n, n)))
          case a: AssignmentTag => a.copy(assignTo = vals.getOrElse(a.assignTo, a.assignTo))
          case t: ForTag if vals.contains(t.item) => t.copy(item = vals(t.item))
          case t: NextTag if vals.contains(t.item) => t.copy(item = vals(t.item))
          case t => t
        }
      )
    )

  def :+:(prev: FuncOp): FuncOp =
    FuncOp.RightAssocSemi.combine(prev, this)
}

object FuncOp {
  type Tree = Cofree[Chain, RawTag]

  def traverseA[A](cf: Tree, init: A)(
    f: (A, RawTag) => (A, Tree)
  ): Eval[(A, Tree)] = {
    val (headA, head) = f(init, cf.head)
    // TODO: it should be in standard library, with some other types
    cf.tail
      .map(_.foldLeft[(A, Chain[Tree])]((headA, head.tailForced)) {
        case ((aggrA, aggrTail), child) =>
          traverseA(child, aggrA)(f).value.map(aggrTail.append)
      })
      .map(_.map(ch => head.copy(tail = Eval.now(ch))))
  }

  // Semigroup for foldRight processing
  object RightAssocSemi extends Semigroup[FuncOp] {

    override def combine(x: FuncOp, y: FuncOp): FuncOp = (x.tree.head, y.tree.head) match {
      case (ParTag, ParTag) => FuncOp(y.tree.copy(tail = (x.tree.tail, y.tree.tail).mapN(_ ++ _)))
      case (XorTag, XorTag) =>
        FuncOp(y.tree.copy(tail = (x.tree.tail, y.tree.tail).mapN(_ ++ _)))
      case (XorTag.LeftBiased, XorTag) =>
        wrap(SeqTag, FuncOp(y.tree.copy(tail = (x.tree.tail, y.tree.tail).mapN(_ ++ _))))
      case (XorTag, ParTag) => FuncOp(Cofree[Chain, RawTag](XorParTag(x, y), Eval.now(Chain.empty)))
      case (_, ParTag | XorTag) =>
        wrap(SeqTag, FuncOp(y.tree.copy(tail = y.tree.tail.map(_.prepend(x.tree)))))
      case (_, XorParTag(xor, par)) =>
        combine(combine(x, xor), par)
      case _ => FuncOpSemigroup.combine(x, y)
    }
  }

  implicit object FuncOpSemigroup extends Semigroup[FuncOp] {

    override def combine(x: FuncOp, y: FuncOp): FuncOp = (x.tree.head, y.tree.head) match {
      case (_, XorParTag(xor, par)) => combine(combine(x, xor), par)
      case (XorParTag(xor, par), _) => combine(combine(xor, par), y)
      case (SeqTag, SeqTag) => FuncOp(y.tree.copy(tail = (x.tree.tail, y.tree.tail).mapN(_ ++ _)))
      case (_, SeqTag) => FuncOp(y.tree.copy(tail = y.tree.tail.map(_.prepend(x.tree))))
      case (SeqTag, _) => FuncOp(x.tree.copy(tail = x.tree.tail.map(_.append(y.tree))))
      case _ => node(SeqTag, Chain(x, y))
    }
  }

  def leaf(tag: RawTag): FuncOp = FuncOp(Cofree[Chain, RawTag](tag, Eval.now(Chain.empty)))

  def wrap(tag: RawTag, child: FuncOp): FuncOp = node(tag, Chain.one(child))

  def node(tag: RawTag, children: Chain[FuncOp]): FuncOp =
    FuncOp(Cofree[Chain, RawTag](tag, Eval.later(children.map(_.tree))))
}
