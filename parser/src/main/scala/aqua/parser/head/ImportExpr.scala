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

import aqua.parser.lexer.Token._
import aqua.parser.lexer.{LiteralToken, ValueToken}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}

import cats.Comonad
import cats.parse.Parser
import cats.~>

case class ImportExpr[F[_]](filename: LiteralToken[F]) extends FilenameExpr[F] {

  override def mapK[K[_]: Comonad](fk: F ~> K): ImportExpr[K] =
    copy(filename.mapK(fk))

  override def toString: String = s"import ${filename.value}"
}

object ImportExpr extends HeaderExpr.Companion {

  override val p: Parser[HeaderExpr[Span.S]] =
    `import` *> ` ` *> ValueToken.string.map(ImportExpr(_))
}
