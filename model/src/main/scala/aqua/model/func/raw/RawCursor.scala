package aqua.model.func.raw

import aqua.model.ValueModel
import aqua.model.cursor.{ChainCursor, ChainZipper}
import cats.Eval
import cats.data.{Chain, NonEmptyList, OptionT}
import cats.free.Cofree
import cats.syntax.traverse._

// Can be heavily optimized by caching parent cursors, not just list of zippers
case class RawCursor(tree: NonEmptyList[ChainZipper[FuncOp.Tree]])
    extends ChainCursor[RawCursor, FuncOp.Tree](RawCursor) {
  def tag: RawTag = current.head
  def parentTag: Option[RawTag] = parent.map(_.head)

  def hasChildren: Boolean =
    current.tailForced.nonEmpty

  def toFirstChild: Option[RawCursor] =
    ChainZipper.first(current.tail.value).map(moveDown)

  def toLastChild: Option[RawCursor] =
    ChainZipper.last(current.tail.value).map(moveDown)

  def tagsPath: NonEmptyList[RawTag] = path.map(_.head)

  def pathOn: List[OnTag] = tagsPath.collect { case o: OnTag =>
    o
  }

  def currentPeerId: Option[ValueModel] = pathOn.headOption.map(_.peerId)

  // Cursor to the last sequentially executed operation, if any
  def lastExecuted: Option[RawCursor] = tag match {
    case XorTag => toFirstChild.flatMap(_.lastExecuted)
    case _: SeqGroupTag => toLastChild.flatMap(_.lastExecuted)
    case ParTag => None
    case _: NoExecTag => None
    case _ => Some(this)
  }

  def firstExecuted: Option[RawCursor] = tag match {
    case _: SeqGroupTag => toLastChild.flatMap(_.lastExecuted)
    case ParTag => None
    case _: NoExecTag => None
    case _ => Some(this)
  }

  /**
   * Sequentially previous cursor
   * @return
   */
  def seqPrev: Option[RawCursor] =
    parentTag.flatMap {
      case _: SeqGroupTag =>
        moveLeft.flatMap(c => c.lastExecuted orElse c.seqPrev)
      case _ => moveUp.flatMap(_.seqPrev)
    }

  def seqNext: Option[RawCursor] =
    parentTag.flatMap {
      case _: SeqGroupTag =>
        moveRight.flatMap(c => c.firstExecuted orElse c.seqNext)
      case _ => moveUp.flatMap(_.seqNext)
    }

  // TODO write a test
  def checkNamesUsedLater(names: Set[String]): Boolean =
    allToRight
      .map(_.current)
      .map(FuncOp(_))
      .exists(_.usesVarNames.value.intersect(names).nonEmpty)

  def pathFromPrev: Chain[ValueModel] =
    seqPrev.fold(Chain.empty[ValueModel])(PathFinder.find(_, this))

  def pathToNext: Chain[ValueModel] = parentTag.flatMap {
    case ParTag =>
      val exports = FuncOp(current).exportsVarNames.value
      if (exports.nonEmpty && checkNamesUsedLater(exports)) seqNext else None
    case XorTag if leftSiblings.nonEmpty => seqNext
    case _ => None
  }.fold(Chain.empty[ValueModel])(PathFinder.find(this, _))

  def cata[A](
    f: RawCursor => OptionT[Eval, ChainZipper[Cofree[Chain, A]]]
  ): Eval[Chain[Cofree[Chain, A]]] =
    f(this).map { case ChainZipper(prev, curr, next) =>
      prev ++ Chain.one(
        curr.copy(tail =
          Eval
            .later(
              Chain.fromSeq(
                toFirstChild
                  .map(fc => LazyList.unfold(fc)(_.moveRight.map(c => c -> c)).prepended(fc))
                  .getOrElse(LazyList.empty)
              )
            )
            .flatMap(_.traverse(_.cata(f)))
            .map(_.flatMap(identity))
            .flatMap(addition => curr.tail.map(_ ++ addition))
        )
      ) ++ next
    }.getOrElse(Chain.empty)

}
