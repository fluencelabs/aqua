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

package aqua.lsp

object Utils {

  def getByPosition(code: String, str: String, position: Int): Option[(Int, Int)] = {
    str.r.findAllMatchIn(code).toList.lift(position).map(r => (r.start, r.end))
  }

  extension [T](o: Option[T]) {

    def tapNone(f: => Unit): Option[T] =
      o.orElse {
        f; None
      }
  }
}
