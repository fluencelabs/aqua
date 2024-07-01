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

package aqua.model.transform.cursor

import cats.data.{Chain, NonEmptyList}

/**
 * Represents a position in a tree as a path of chain zippers
 */
abstract class ChainCursor[C <: ChainCursor[C, T], T](make: NonEmptyList[ChainZipper[T]] => C) {
  self =>

  // All parents to this element, going up
  val tree: NonEmptyList[ChainZipper[T]]

  // Parent element, if not at root
  def parent: Option[T] = moveUp.map(_.current)

  // The closest element
  def current: T = tree.head.current

  // This level is represented as ChainZipper, so we have left and right siblings
  def leftSiblings: Chain[T] = tree.head.prev
  def rightSiblings: Chain[T] = tree.head.next

  // Modify the parent element, if it exists
  def mapParent(f: T => T): C =
    make(
      NonEmptyList(
        tree.head,
        tree.tail match {
          case h :: t => h.copy(current = f(h.current)) :: t
          case t => t
        }
      )
    )

  // Path to this position: just drop siblings
  lazy val path: NonEmptyList[T] = tree.map(_.current)

  // Move cursor up
  // TODO: ensure this cursor's data is cached properly
  def moveUp: Option[C] = NonEmptyList.fromList(tree.tail).map(make)

  // Path to root, in form of Cursors; this is skipped
  val pathToRoot: LazyList[C] = LazyList.unfold(this)(_.moveUp.map(c => c -> c))

  // Move down: need a ChainZipper that's below
  def moveDown(focusOn: ChainZipper[T]): C = make(focusOn :: tree)

  // Move to previous sibling, if any; otherwise, go to parent and move left from it
  // Is it correct?
  def moveLeft: Option[C] =
    toPrevSibling orElse moveUp.flatMap(_.moveLeft)

  def moveRight: Option[C] =
    toNextSibling orElse moveUp.flatMap(_.moveRight)

  def toNextSibling: Option[C] = tree.head.moveRight.map(p => make(tree.copy(p)))

  def toPrevSibling: Option[C] = tree.head.moveLeft.map(p => make(tree.copy(p)))

  def nextSiblings: LazyList[C] =
    LazyList.unfold(this)(_.toNextSibling.map(c => c -> c))

  def allToLeft: LazyList[C] =
    LazyList.unfold(this)(_.moveLeft.map(c => c -> c))

  def allToRight: LazyList[C] =
    LazyList.unfold(this)(_.moveRight.map(c => c -> c))
}
