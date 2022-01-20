package aqua.model.transform.topology

import aqua.model.*
import cats.Eval
import cats.data.{Chain, NonEmptyList, OptionT}
import aqua.model.transform.cursor.*
import cats.syntax.traverse.*
import cats.syntax.show.*
import cats.free.Cofree
import scribe.Logging

// Can be heavily optimized by caching parent cursors, not just list of zippers
case class OpModelTreeCursor(
  tree: NonEmptyList[ChainZipper[OpModel.Tree]],
  cachedParent: Option[OpModelTreeCursor] = None
) extends ChainCursor[OpModelTreeCursor, OpModel.Tree](OpModelTreeCursor.apply(_, None))
    with Logging {

  override def moveUp: Option[OpModelTreeCursor] = cachedParent.orElse(super.moveUp)

  override lazy val toPrevSibling: Option[OpModelTreeCursor] =
    super.toPrevSibling.map(_.copy(cachedParent = cachedParent))

  override lazy val toNextSibling: Option[OpModelTreeCursor] =
    super.toNextSibling.map(_.copy(cachedParent = cachedParent))

  override def moveDown(focusOn: ChainZipper[OpModel.Tree]): OpModelTreeCursor =
    super.moveDown(focusOn).copy(cachedParent = Some(this))

  def op: OpModel = current.head

  def parentOp: Option[OpModel] = parent.map(_.head)

  lazy val hasChildren: Boolean =
    current.tailForced.nonEmpty

  lazy val toFirstChild: Option[OpModelTreeCursor] =
    ChainZipper.first(current.tail.value).map(moveDown)

  lazy val toLastChild: Option[OpModelTreeCursor] =
    ChainZipper.last(current.tail.value).map(moveDown)

  lazy val children: LazyList[OpModelTreeCursor] =
    LazyList.unfold(toFirstChild)(_.map(c => c -> c.toNextSibling))

  def findInside(f: OpModelTreeCursor => Boolean): LazyList[OpModelTreeCursor] =
    children.flatMap(_.findInside(f)).prependedAll(Option.when(f(this))(this))

  lazy val topology: Topology = Topology.make(this)

  lazy val tagsPath: NonEmptyList[OpModel] = path.map(_.head)

  // Whether the current branch contains any AIR-executable code or not
  lazy val isNoExec: Boolean =
    op match {
      case _: NoExecModel => true
      case _: GroupOpModel => children.forall(_.isNoExec)
      case _ => false
    }

  def hasExecLater: Boolean =
    !allToRight.forall(_.isNoExec)

  // Whether variables exported from this branch are used later in the code or not
  def exportsUsedLater: Boolean =
    OpModel.exportsVarNames(current).map(ns => ns.nonEmpty && checkNamesUsedLater(ns)).value

  // TODO write a test
  def checkNamesUsedLater(names: Set[String]): Boolean =
    allToRight
      .map(_.current)
      .map(OpModel.usesVarNames)
      .exists(_.value.intersect(names).nonEmpty)

  def cata[A](wrap: ChainZipper[Cofree[Chain, A]] => Chain[Cofree[Chain, A]])(
    folder: OpModelTreeCursor => OptionT[Eval, ChainZipper[Cofree[Chain, A]]]
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

  override def toString: String =
    s"[${tree.head.prev.length}]${op.show} /: ${moveUp.getOrElse("(|)")}"
}
