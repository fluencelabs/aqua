/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.model.transform.topology

import aqua.model.*
import aqua.model.transform.cursor.*

import cats.Eval
import cats.data.{Chain, NonEmptyList, OptionT}
import cats.syntax.traverse.*
import cats.syntax.show.*
import cats.syntax.foldable.*
import cats.syntax.apply.*
import cats.instances.lazyList.*
import cats.syntax.applicative.*
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

  override lazy val nextSiblings: LazyList[OpModelTreeCursor] =
    super.nextSiblings.map(_.copy(cachedParent = cachedParent))

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

  lazy val subtree: LazyList[OpModelTreeCursor] =
    children.flatMap(c => c #:: c.subtree).prepended(this)

  def findInside(f: OpModelTreeCursor => Boolean): LazyList[OpModelTreeCursor] =
    subtree.filter(f)

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
  def exportsUsedLater: Boolean = (
    namesUsedLater,
    OpModel.exportsVarNames(current)
  ).mapN(_ intersect _).value.nonEmpty

  def namesUsedLater: Eval[Set[String]] =
    allToRight
      .map(_.current)
      .map(OpModel.usesVarNames)
      .combineAll

  // Check that exports of this subtree are used later in the code
  // Do not take into account subtrees for which the filter returns false
  def exportsUsedLaterFilter(
    filter: OpModelTreeCursor => Boolean
  ): Eval[Boolean] = (
    cata((cur, childs: Chain[Set[String]]) =>
      Eval.later(
        if (filter(cur))
          childs.combineAll ++
            // TODO: Move to OpModel
            cur.op.exportsVarNames --
            cur.op.restrictsVarNames
        else Set.empty
      )
    ),
    namesUsedLater
  ).mapN(_ intersect _).map(_.nonEmpty)

  def cata[A](f: (OpModelTreeCursor, Chain[A]) => Eval[A]): Eval[A] =
    for {
      childs <- Chain.fromSeq(children).traverse(_.cata(f))
      res <- f(this, childs)
    } yield res

  def traverse[A](wrap: ChainZipper[Cofree[Chain, A]] => Chain[Cofree[Chain, A]])(
    folder: OpModelTreeCursor => OptionT[Eval, ChainZipper[Cofree[Chain, A]]]
  ): Eval[Chain[Cofree[Chain, A]]] =
    folder(this).map { case cz @ ChainZipper(_, curr, _) =>
      val updatedTail = for {
        childs <- Eval.later(Chain.fromSeq(children))
        addition <- childs.flatTraverse(_.traverse(wrap)(folder))
        tail <- curr.tail
      } yield tail ++ addition

      wrap(cz.copy(current = curr.copy(tail = updatedTail)))
    }.getOrElse(Chain.empty).memoize

  override def toString: String =
    s"[${tree.head.prev.length}] ${op.show} /: ${moveUp.getOrElse("(|)")}"
}
