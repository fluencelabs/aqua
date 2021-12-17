package aqua.model.transform.topology

import aqua.model.ValueModel
import aqua.model.func.raw.*
import aqua.model.func.raw.FuncOp.Tree
import cats.Eval
import cats.data.{Chain, NonEmptyList, OptionT}
import aqua.model.transform.cursor.*
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

  lazy val topology: Topology = Topology.make(this)

  lazy val tagsPath: NonEmptyList[RawTag] = path.map(_.head)

  // Cursor to the last sequentially executed operation, if any
  @deprecated("Use topology", "15.12.2021")
  lazy val lastExecuted: Option[RawCursor] = tag match {
    case XorTag => toFirstChild.flatMap(_.lastExecuted)
    case _: SeqGroupTag => toLastChild.flatMap(_.lastExecuted)
    case _: ParGroupTag =>
      None // ParGroup builds exit path within itself; there's no "lastExecuted", they are many
    case _: NoExecTag => moveLeft.flatMap(_.lastExecuted)
    case _ => Some(this)
  }

  @deprecated("Use topology", "15.12.2021")
  lazy val firstExecuted: Option[RawCursor] = tag match {
    case _: SeqGroupTag =>
      toFirstChild.flatMap(_.firstExecuted)
    case _: ParGroupTag =>
      None // As many branches are executed simultaneously, no definition of first
    case _: NoExecTag => moveRight.flatMap(_.firstExecuted)
    case _ => Some(this)
  }

  @deprecated("Use topology", "15.12.2021")
  lazy val firstExecutedPathOn: Option[List[OnTag]] =
    firstExecuted
      .map(_.topology.pathOn)
      .orElse(
        tag match {
          case _: ParGroupTag =>
            children
              .flatMap(_.firstExecutedPathOn)
              .map(_.reverse)
              .reduceLeftOption[List[OnTag]] { case (a, b) =>
                (a zip b).takeWhile(_ == _).map(_._1)
              }
              .map(_.reverse)
          case _ => None
        }
      )

  /**
   * Sequentially previous cursor
   * @return
   */
  @deprecated("Use topology", "15.12.2021")
  lazy val seqPrev: Option[RawCursor] =
    parentTag.flatMap {
      case p: SeqGroupTag if leftSiblings.nonEmpty =>
        toPrevSibling.flatMap(c => c.lastExecuted orElse c.seqPrev)
      case p =>
        moveUp.flatMap(_.seqPrev)
    }

  @deprecated("Use topology", "15.12.2021")
  lazy val seqNext: Option[RawCursor] =
    parentTag.flatMap {
      case _: SeqGroupTag if rightSiblings.nonEmpty =>
        toNextSibling.flatMap(c => c.firstExecuted orElse c.seqNext)
      case _ => moveUp.flatMap(_.seqNext)
    }

  lazy val isNoExec: Boolean =
    tag match {
      case _: NoExecTag => true
      case _: GroupTag => children.forall(_.isNoExec)
      case _ => false
    }

  def hasExecLater: Boolean =
    !allToRight.forall(_.isNoExec)

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
                .unfold(folderCursor) { _.toNextSibling.map(cursor => cursor -> cursor) }
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
