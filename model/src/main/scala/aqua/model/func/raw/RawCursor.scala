package aqua.model.func.raw

import aqua.model.ValueModel
import aqua.model.cursor.{ChainCursor, ChainZipper}
import cats.Eval
import cats.data.{Chain, NonEmptyList, OptionT}
import cats.free.Cofree
import cats.syntax.traverse._
import wvlet.log.LogSupport

// Can be heavily optimized by caching parent cursors, not just list of zippers
case class RawCursor(tree: NonEmptyList[ChainZipper[FuncOp.Tree]])
    extends ChainCursor[RawCursor, FuncOp.Tree](RawCursor) with LogSupport {
  def tag: RawTag = current.head
  def parentTag: Option[RawTag] = parent.map(_.head)

  def hasChildren: Boolean =
    current.tailForced.nonEmpty

  lazy val toFirstChild: Option[RawCursor] =
    ChainZipper.first(current.tail.value).map(moveDown)

  lazy val toLastChild: Option[RawCursor] =
    ChainZipper.last(current.tail.value).map(moveDown)

  lazy val tagsPath: NonEmptyList[RawTag] = path.map(_.head)

  lazy val pathOn: List[OnTag] = tagsPath.collect { case o: OnTag =>
    o
  }

  lazy val rootOn: Option[RawCursor] = moveUp
    .flatMap(_.rootOn)
    .orElse(tag match {
      case _: OnTag =>
        Some(this)
      case _ => None
    })

  lazy val currentPeerId: Option[ValueModel] = pathOn.headOption.map(_.peerId)

  // Cursor to the last sequentially executed operation, if any
  lazy val lastExecuted: Option[RawCursor] = tag match {
    case XorTag => toFirstChild.flatMap(_.lastExecuted)
    case _: SeqGroupTag => toLastChild.flatMap(_.lastExecuted)
    case ParTag => None
    case _: NoExecTag => None
    case _ => Some(this)
  }

  lazy val firstExecuted: Option[RawCursor] = tag match {
    case _: SeqGroupTag => toLastChild.flatMap(_.lastExecuted)
    case ParTag => None
    case _: NoExecTag => None
    case _ => Some(this)
  }

  /**
   * Sequentially previous cursor
   * @return
   */
  lazy val seqPrev: Option[RawCursor] =
    parentTag.flatMap {
      case p: SeqGroupTag =>
        info(s"Parent is $p, resolve ${moveLeft.getOrElse("(|)")}")
        toPrevSibling.flatMap(c => c.lastExecuted orElse c.seqPrev)
      case _ =>
        error(s"GO UP TO ${moveUp.getOrElse("(|)")}")
        moveUp.flatMap(_.seqPrev)
    }

  lazy val seqNext: Option[RawCursor] =
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

  lazy val pathFromPrev: Chain[ValueModel] = {

    parentTag.fold(Chain.empty[ValueModel]) {
      case _: GroupTag =>
        info(s"Lookup prev path: $this")
        seqPrev
          .orElse(rootOn)
          .fold(Chain.empty[ValueModel])(PathFinder.find(_, this))
      case _ =>
        info(s"Skip: $this")
        Chain.empty
    }
  }

  lazy val pathToNext: Chain[ValueModel] = parentTag.flatMap {
    case ParTag =>
      val exports = FuncOp(current).exportsVarNames.value
      if (exports.nonEmpty && checkNamesUsedLater(exports)) seqNext else None
    case XorTag if leftSiblings.nonEmpty => seqNext
    case _ => None
  }.fold(Chain.empty[ValueModel])(PathFinder.find(this, _))

  def cata[A](wrap: ChainZipper[Cofree[Chain, A]] => Chain[Cofree[Chain, A]])(
    f: RawCursor => OptionT[Eval, ChainZipper[Cofree[Chain, A]]]
  ): Eval[Chain[Cofree[Chain, A]]] =
    f(this).map { case ChainZipper(prev, curr, next) =>
      val inner = curr.copy(tail =
        Eval
          .later(
            Chain.fromSeq(
              toFirstChild
                .map(fc =>
                  LazyList
                    .unfold(fc) { _.toNextSibling.map(c => c -> c) }
                    .prepended(fc)
                )
                .getOrElse(LazyList.empty)
            )
          )
          .flatMap(_.traverse(_.cata(wrap)(f)))
          .map(_.flatMap(identity))
          .flatMap(addition => curr.tail.map(_ ++ addition))
      )
      wrap(ChainZipper(prev, inner, next))
    }.getOrElse(Chain.empty).memoize

  override def toString: String = s"$tag /: ${moveUp.getOrElse("(|)")}"
}
