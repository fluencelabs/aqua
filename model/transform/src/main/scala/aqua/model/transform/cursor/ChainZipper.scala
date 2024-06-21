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

import cats.data.Chain
import cats.free.Cofree

case class ChainZipper[T](prev: Chain[T], current: T, next: Chain[T]) {

  def chain: Chain[T] = (prev :+ current) ++ next

  def moveLeft: Option[ChainZipper[T]] =
    prev.initLast.map { case (init, last) =>
      ChainZipper(init, last, current +: next)
    }

  def moveRight: Option[ChainZipper[T]] =
    next.uncons.map { case (head, tail) =>
      ChainZipper(prev :+ current, head, tail)
    }

  def replaceInjecting(cz: ChainZipper[T]): ChainZipper[T] =
    copy(prev ++ cz.prev, cz.current, cz.next ++ next)
}

object ChainZipper {
  def one[T](el: T): ChainZipper[T] = ChainZipper(Chain.empty, el, Chain.empty)

  def first[T](chain: Chain[T]): Option[ChainZipper[T]] =
    chain.uncons.map { case (current, next) =>
      ChainZipper(Chain.empty, current, next)
    }

  def last[T](chain: Chain[T]): Option[ChainZipper[T]] =
    chain.initLast.map { case (prev, current) =>
      ChainZipper(prev, current, Chain.empty)
    }

  def traverseChain[T](chain: Chain[T], prev: Chain[T] = Chain.empty): Chain[ChainZipper[T]] =
    chain.uncons.fold(Chain.empty[ChainZipper[T]]) { case (t, next) =>
      ChainZipper(prev, t, next) +: traverseChain(next, prev :+ t)
    }

  /**
   * Walks through the given chain as a zipper, applying the given function to it
   *
   * @param chain Chain to traverse and map
   * @param prev Accumulator: previous steps
   * @param f Function that converts a zipper (meaning an element with its horizontal context) to element
   * @tparam T Type
   * @return Resulting chain
   */
  def traverseChainMap[T](chain: Chain[T], prev: Chain[T] = Chain.empty)(
    f: ChainZipper[T] => Option[T]
  ): Chain[T] =
    chain.uncons.fold(Chain.empty[T]) { case (t, next) =>
      f(ChainZipper(prev, t, next)).fold(traverseChainMap(next, prev)(f))(r =>
        r +: traverseChainMap(next, prev :+ r)(f)
      )
    }

  object Matchers {

    object `current` {
      def unapply[T](cz: ChainZipper[T]): Option[T] = Some(cz.current)
    }

    object `head` {
      def unapply[F[_], T](cz: ChainZipper[Cofree[F, T]]): Option[T] = Some(cz.current.head)
    }
  }
}
