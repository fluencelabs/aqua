package aqua.raw.ops

import aqua.raw.Raw
import aqua.raw.ops
import aqua.raw.value.{ValueRaw, VarRaw}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import cats.instances.tuple.*
import cats.kernel.Semigroup
import cats.syntax.apply.*
import cats.syntax.functor.*

case class FuncOp(tree: Cofree[Chain, RawTag]) extends Raw {
  def head: RawTag = tree.head

  lazy val isRightAssoc: Boolean = head match {
    case XorTag | ParTag => true
    case _: XorParTag => true
    case _ => false
  }

  def cata[T](folder: (RawTag, Chain[T]) => Eval[T]): Eval[T] =
    Cofree.cata(tree)(folder)

  def definesVarNames: Eval[Set[String]] = cata[Set[String]] {
    case (CallArrowTag(_, Call(_, exportTo)), acc) if exportTo.nonEmpty =>
      Eval.later(acc.foldLeft(exportTo.map(_.name).toSet)(_ ++ _))
    case (PushToStreamTag(_, exportTo), acc) =>
      Eval.later(acc.foldLeft(Set(exportTo.name))(_ ++ _))
    case (CanonicalizeTag(_, exportTo), acc) =>
      Eval.later(acc.foldLeft(Set(exportTo.name))(_ ++ _))
    case (CallServiceTag(_, _, Call(_, exportTo)), acc) if exportTo.nonEmpty =>
      Eval.later(acc.foldLeft(exportTo.map(_.name).toSet)(_ ++ _))
    case (NextTag(exportTo), acc) => Eval.later(acc.foldLeft(Set(exportTo))(_ ++ _))
    case (RestrictionTag(name, _), acc) =>
      Eval.later(acc.foldLeft(Set(name))(_ ++ _))
    case (_, acc) => Eval.later(acc.foldLeft(Set.empty[String])(_ ++ _))
  }

  def exportsVarNames: Eval[Set[String]] = cata[Set[String]] {
    case (CallArrowTag(_, Call(_, exportTo)), acc) if exportTo.nonEmpty =>
      Eval.later(acc.foldLeft(exportTo.map(_.name).toSet)(_ ++ _))
    case (CallServiceTag(_, _, Call(_, exportTo)), acc) if exportTo.nonEmpty =>
      Eval.later(acc.foldLeft(exportTo.map(_.name).toSet)(_ ++ _))
    case (PushToStreamTag(_, exportTo), acc) =>
      Eval.later(acc.foldLeft(Set(exportTo.name))(_ ++ _))
    case (RestrictionTag(name, _), acc) =>
      Eval.later(acc.foldLeft(Set.empty[String])(_ ++ _) - name)
    case (CanonicalizeTag(_, exportTo), acc) =>
      Eval.later(acc.foldLeft(Set(exportTo.name))(_ ++ _))
    case (_, acc) => Eval.later(acc.foldLeft(Set.empty[String])(_ ++ _))
  }

  // TODO: as it is used for checking of intersection, make it a lazy traverse with fail-fast
  def usesVarNames: Eval[Set[String]] = cata[Set[String]] {
    case (CallArrowTag(_, call), acc) =>
      Eval.later(acc.foldLeft(call.argVarNames)(_ ++ _))
    case (CallServiceTag(_, _, call), acc) =>
      Eval.later(acc.foldLeft(call.argVarNames)(_ ++ _))
    case (PushToStreamTag(operand, _), acc) =>
      Eval.later(acc.foldLeft(operand.usesVarNames)(_ ++ _))
    case (RestrictionTag(name, _), acc) =>
      Eval.later(acc.foldLeft(Set.empty[String])(_ ++ _) - name)
    case (CanonicalizeTag(operand, _), acc) =>
      Eval.later(acc.foldLeft(operand.usesVarNames)(_ ++ _))
    case (ForTag(_, iterable), acc) =>
      Eval.later(acc.foldLeft(iterable.usesVarNames)(_ ++ _))
    case (tag, acc) => Eval.later(acc.foldLeft(tag.usesVarNames)(_ ++ _))
  }

  def resolveValues(vals: Map[String, ValueRaw]): FuncOp =
    FuncOp(tree.map[RawTag](_.mapValues(_.resolveWith(vals))))

  def rename(vals: Map[String, String]): FuncOp = {
    if (vals.isEmpty)
      this
    else
      FuncOp(
        tree.map[RawTag](op =>
          op.mapValues {
            case v: VarRaw if vals.contains(v.name) => v.copy(name = vals(v.name))
            case v => v
          } match {
            case c: CallArrowTag => c.copy(call = c.call.mapExport(n => vals.getOrElse(n, n)))
            case c: CallServiceTag => c.copy(call = c.call.mapExport(n => vals.getOrElse(n, n)))
            case a: PushToStreamTag =>
              a.copy(exportTo = a.exportTo.mapName(n => vals.getOrElse(n, n)))
            case a: CanonicalizeTag =>
              a.copy(exportTo = a.exportTo.mapName(n => vals.getOrElse(n, n)))
            case a: AssignmentTag => a.copy(assignTo = vals.getOrElse(a.assignTo, a.assignTo))
            case t: ForTag if vals.contains(t.item) => t.copy(item = vals(t.item))
            case t: NextTag if vals.contains(t.item) => t.copy(item = vals(t.item))
            case t: RestrictionTag if vals.contains(t.name) => t.copy(name = vals(t.name))
            case t => t
          }
        )
      )
  }

  def :+:(prev: FuncOp): FuncOp =
    FuncOp.RightAssocSemi.combine(prev, this)

  // Function body must be fixed before function gets resolved
  def fixXorPar: FuncOp =
    ops.FuncOp(cata[Cofree[Chain, RawTag]] {
      case (XorParTag(left, right), _) =>
        Eval.now(
          FuncOps
            .par(
              FuncOp.wrap(XorTag, left),
              right
            )
            .tree
        )

      case (head, tail) => Eval.now(Cofree(head, Eval.now(tail)))
    }.value)
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
          traverseA(child, aggrA)(f).value match { case (a, tree) => (a, aggrTail.append(tree)) }
      })
      .map { case (a, ch) => (a, head.copy(tail = Eval.now(ch))) }
  }

  // Semigroup for foldRight processing
  object RightAssocSemi extends Semigroup[FuncOp] {

    override def combine(x: FuncOp, y: FuncOp): FuncOp = (x.tree.head, y.tree.head) match {
      case (_: ParGroupTag, ParTag) =>
        FuncOp(y.tree.copy(tail = (x.tree.tail, y.tree.tail).mapN(_ ++ _)))
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
