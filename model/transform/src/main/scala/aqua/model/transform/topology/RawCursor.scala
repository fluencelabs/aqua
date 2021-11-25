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
case class RawCursor(tree: NonEmptyList[ChainZipper[FuncOp.Tree]], cachedParent: Option[RawCursor])
    extends ChainCursor[RawCursor, FuncOp.Tree](RawCursor.apply(_, None)) with Logging {

  override def moveUp: Option[RawCursor] = cachedParent.orElse(super.moveUp)

  override def toPrevSibling: Option[RawCursor] =
    super.toPrevSibling.map(_.copy(cachedParent = cachedParent))

  override def toNextSibling: Option[RawCursor] =
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

  def isOnTag: Boolean =
    tag match {
      case _: OnTag => true
      case _ => false
    }

  def isForTag: Boolean =
    tag match {
      case _: ForTag => true
      case _ => false
    }

  lazy val tagsPath: NonEmptyList[RawTag] = path.map(_.head)

  lazy val pathOn: List[OnTag] = tagsPath.collect { case o: OnTag =>
    o
  }

  // Assume that the very first tag is `on` tag
  lazy val rootOn: Option[RawCursor] = moveUp
    .flatMap(_.rootOn)
    .orElse(tag match {
      case _: OnTag =>
        Some(this)
      case _ => None
    })

  // The closest peerId
  lazy val currentPeerId: Option[ValueModel] =
    pathOn.headOption.map(_.peerId)

  // Cursor to the last sequentially executed operation, if any
  lazy val lastExecuted: Option[RawCursor] = tag match {
    case XorTag => toFirstChild.flatMap(_.lastExecuted)
    case _: SeqGroupTag => toLastChild.flatMap(_.lastExecuted)
    case _: ParGroupTag =>
      None // ParGroup builds exit path within itself; there's no "lastExecuted", they are many
    case _: NoExecTag => moveLeft.flatMap(_.lastExecuted)
    case _ => Some(this)
  }

  lazy val firstExecuted: Option[RawCursor] = tag match {
    case _: SeqGroupTag =>
      toFirstChild.flatMap(_.firstExecuted)
    case _: ParGroupTag =>
      None // As many branches are executed simultaneously, no definition of first
    case _: NoExecTag => moveRight.flatMap(_.firstExecuted)
    case _ => Some(this)
  }

  /**
   * Sequentially previous cursor
   * @return
   */
  lazy val seqPrev: Option[RawCursor] =
    parentTag.flatMap {
      case p: SeqGroupTag if leftSiblings.nonEmpty =>
        toPrevSibling.flatMap(c => c.lastExecuted orElse c.seqPrev)
      case p =>
        moveUp.flatMap(_.seqPrev)
    }

  lazy val seqNext: Option[RawCursor] =
    parentTag.flatMap {
      case _: SeqGroupTag if rightSiblings.nonEmpty =>
        toNextSibling.flatMap(c => c.firstExecuted orElse c.seqNext)
      case _ => moveUp.flatMap(_.seqNext)
    }

  // TODO write a test
  def checkNamesUsedLater(names: Set[String]): Boolean =
    allToRight
      .map(_.current)
      .map(FuncOp(_))
      .exists(_.usesVarNames.value.intersect(names).nonEmpty)

  lazy val pathFromPrev: Chain[ValueModel] = tag match {
    case ForTag(item, _) =>
      // Move the stable part of path into for loop outside the for loop -- https://github.com/fluencelabs/aqua/issues/205

      // Find the first executed element: in case of for...par, we need to dive into that par with toFirstChild (twice)
      firstExecuted
        .orElse(toFirstChild.flatMap(_.toFirstChild).flatMap(_.firstExecuted))
        .flatMap(fc =>
          // Find the first OnTag inside this For, if any
          LazyList
            .unfold(fc)(c =>
              c.moveUp
                .filterNot(_.pathOn == pathOn)
                .map(c => c -> c)
            )
            .collectFirst {
              case c if c.isOnTag =>
                // For the deepest On, get the path
                c.pathFromPrevD().takeWhile(v => !ValueModel.varName(v).contains(item))
            }
        )
        .getOrElse(Chain.empty)

    case _ =>
      // The stable part of path inside the loop was already moved outside -- take it into account
      // This means that this element contains the first
      val p = pathFromPrevD()
      LazyList
        .unfold(this)(c => c.moveUp.map(rc => rc -> rc))
        .collectFirst {
          case c if c.isForTag =>
            c.pathFromPrev
        }
      p
  }

  def pathFromPrevD(forExit: Boolean = false): Chain[ValueModel] =
    parentTag.fold(Chain.empty[ValueModel]) {
      case _: GroupTag =>
        seqPrev
          .orElse(rootOn)
          .fold(Chain.empty[ValueModel])(PathFinder.find(_, this, isExit = forExit))

      case _ =>
        Chain.empty
    }

  lazy val pathToNext: Chain[ValueModel] = parentTag.fold(Chain.empty[ValueModel]) {
    case _: ParGroupTag =>
      val exports = FuncOp(current).exportsVarNames.value
      if (exports.nonEmpty && checkNamesUsedLater(exports))
        seqNext.fold(Chain.empty[ValueModel])(nxt =>
          PathFinder.find(this, nxt) ++
            // we need to "wake" the target peer to enable join behaviour
            Chain.fromOption(nxt.currentPeerId)
        )
      else Chain.empty
    case XorTag if leftSiblings.nonEmpty =>
      lastExecuted
        .flatMap(le =>
          seqNext
            .map(nxt => PathFinder.find(le, nxt, isExit = true) -> nxt)
            .flatMap {
              case (path, nxt) if path.isEmpty && currentPeerId == nxt.currentPeerId =>
                nxt.pathFromPrevD(true).reverse.initLast.map(_._1)
              case (path, nxt) =>
                path.initLast.map {
                  case (init, last)
                      if nxt.pathFromPrevD(forExit = true).headOption.contains(last) =>
                    init
                  case (init, last) => init :+ last
                }
            }
        )
        .getOrElse(Chain.empty)
    case _ =>
      Chain.empty
  }

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
