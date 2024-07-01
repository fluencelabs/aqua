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

package aqua.helpers.syntax

import scala.annotation.tailrec

object list {

  extension [A](l: List[A]) {

    def updateFirst[B >: A](p: A => Boolean, f: A => B): List[B] = {
      @tailrec
      def update(left: List[B], right: List[A]): List[B] =
        right match {
          case a :: tail if p(a) => left.reverse ::: f(a) :: tail
          case a :: tail => update(a :: left, tail)
          case Nil => left.reverse
        }

      update(Nil, l)
    }
  }
}
