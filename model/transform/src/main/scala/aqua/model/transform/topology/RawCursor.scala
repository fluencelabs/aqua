package aqua.model.transform.topology

import aqua.raw.ops.*
import aqua.raw.ops.FuncOp.Tree
import cats.Eval
import cats.data.{Chain, NonEmptyList, OptionT}
import aqua.model.transform.cursor.*
import aqua.raw.ops
import aqua.raw.ops.{FuncOp, GroupTag, NoExecTag, RawTag}
import cats.syntax.traverse.*
import cats.free.Cofree
import scribe.Logging

// Can be heavily optimized by caching parent cursors, not just list of zippers
case class RawCursor(
  tree: NonEmptyList[ChainZipper[FuncOp.Tree]],
  cachedParent: Option[RawCursor] = None
) extends ChainCursor[RawCursor, FuncOp.Tree](RawCursor.apply(_, None)) with Logging {

  override def moveUp: Option[RawCursor] = cachedParent.orElse(super.moveUp)

  override lazy val toPrevSibling: Option[RawCursor] =
    super.toPrevSibling.map(_.copy(cachedParent = cachedParent))

  override lazy val toNextSibling: Option[RawCursor] =
    super.toNextSibling.map(_.copy(cachedParent = cachedParent))

  override def moveDown(focusOn: ChainZipper[Tree]): RawCursor =
    super.moveDown(focusOn).copy(cachedParent = Some(this))

  def tag: RawTag = current.head

  def parentTag: Option[RawTag] = parent.map(_.head)

  lazy val hasChildren: Boolean =
    current.tailForced.nonEmpty

  lazy val toFirstChild: Option[RawCursor] =
    ChainZipper.first(current.tail.value).map(moveDown)

  lazy val toLastChild: Option[RawCursor] =
    ChainZipper.last(current.tail.value).map(moveDown)

  lazy val children: LazyList[RawCursor] =
    LazyList.unfold(toFirstChild)(_.map(c => c -> c.toNextSibling))

  def findInside(f: RawCursor => Boolean): LazyList[RawCursor] =
    children.flatMap(_.findInside(f)).prependedAll(Option.when(f(this))(this))

  lazy val topology: Topology = Topology.make(this)

  lazy val tagsPath: NonEmptyList[RawTag] = path.map(_.head)

  // Whether the current branch contains any AIR-executable code or not
  lazy val isNoExec: Boolean =
    tag match {
      case _: NoExecTag => true
      case _: GroupTag => children.forall(_.isNoExec)
      case _ => false
    }

  def hasExecLater: Boolean =
    !allToRight.forall(_.isNoExec)

  // Whether variables exported from this branch are used later in the code or not
  def exportsUsedLater: Boolean =
    FuncOp(current).exportsVarNames.map(ns => ns.nonEmpty && checkNamesUsedLater(ns)).value

  // TODO write a test
  def checkNamesUsedLater(names: Set[String]): Boolean =
    allToRight
      .map(_.current)
      .map(FuncOp(_))
      .exists(_.usesVarNames.value.intersect(names).nonEmpty)

  def cata[A](wrap: ChainZipper[Cofree[Chain, A]] => Chain[Cofree[Chain, A]])(
    folder: RawCursor => OptionT[Eval, ChainZipper[Cofree[Chain, A]]]
  ): Eval[Chain[Cofree[Chain, A]]] =
    folder(this).map { case ChainZipper(prev, curr, next) =>
      val tail = Eval.later {
        Chain.fromSeq(
          toFirstChild
            .map(folderCursor =>
              LazyList
                .unfold(folderCursor) {
                  _.toNextSibling.map(cursor => cursor -> cursor)
                }
                .prepended(folderCursor)
            )
            .getOrElse(LazyList.empty)
        )
      }
      // TODO: this can be parallelized
        .flatMap(_.traverse(_.cata(wrap)(folder)))
        .map(_.flatMap(identity))
        .flatMap(addition => curr.tail.map(_ ++ addition))

      val inner = curr.copy(tail = tail)
      wrap(ChainZipper(prev, inner, next))
    }.getOrElse(Chain.empty).memoize

  override def toString: String = s"$tag /: ${moveUp.getOrElse("(|)")}"
}
