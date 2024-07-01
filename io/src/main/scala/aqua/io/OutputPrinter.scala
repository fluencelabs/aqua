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

package aqua.io

import cats.effect.std.Console

// Uses to print outputs in CLI
// TODO: add F[_], cause it is effect
object OutputPrinter {

  def print(str: String): Unit = {
    println(str)
  }

  def errorF[F[_]: Console](str: String): F[Unit] = {
    Console[F].errorln(scala.Console.RED + str + scala.Console.RESET)
  }
}
