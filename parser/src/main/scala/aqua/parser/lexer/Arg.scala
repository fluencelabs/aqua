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

package aqua.parser.lexer

import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import Token._
import cats.Comonad
import cats.parse.{Parser => P}

case class Arg[F[_]](name: Name[F], `type`: TypeToken[F])

object Arg {

  val p: P[Arg[Span.S]] =
    ((Name.p <* ` : `) ~ TypeToken.`typedef`).map { case (name, t) =>
      Arg(name, t)
    }
}
