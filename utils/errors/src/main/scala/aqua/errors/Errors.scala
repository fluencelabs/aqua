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

package aqua.errors

import sourcecode.{Enclosing, FileName, Line}

import scala.util.control.NoStackTrace

object Errors {

  /**
   * Internal error that should never happen.
   * Use in case of broken invariants.
   */
  def internalError(
    msg: String
  )(using file: FileName, line: Line, enclosing: Enclosing): Nothing = {
    throw new RuntimeException(
      s"Internal aqua compiler error:\n$msg" +
        s"\nat ${file.value}:${line.value} in ${enclosing.value}.\n" +
        s"Please report this issue to https://github.com/fluencelabs/aqua."
    ) with NoStackTrace // Stack trace is rather useless here
  }
}
