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

package aqua.parser.head

import aqua.parser.Ast
import aqua.parser.lexer.Token
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.Show
import cats.data.Chain
import cats.free.Cofree
import cats.parse.Parser as P
import cats.{Comonad, Eval}
import cats.~>

trait HeaderExpr[S[_]] {
  def token: Token[S]

  def mapK[K[_]: Comonad](fk: S ~> K): HeaderExpr[K]
}

object HeaderExpr {

  trait Companion {
    def p: P[HeaderExpr[Span.S]]
  }

  given [S[_]]: Show[HeaderExpr[S]] with {
    // TODO: Make it better
    def show(e: HeaderExpr[S]): String = e.toString
  }
}
