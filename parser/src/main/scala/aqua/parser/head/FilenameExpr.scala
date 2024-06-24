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

import aqua.helpers.ext.Extension
import aqua.parser.lexer.{LiteralToken, Token}
import cats.Comonad
import cats.~>

trait FilenameExpr[F[_]] extends HeaderExpr[F] {
  def filename: LiteralToken[F]

  override def token: Token[F] = filename

  def fileValue: String = {
    val raw = filename.value.drop(1).dropRight(1)
    Extension.add(raw)
  }

  override def mapK[K[_]: Comonad](fk: F ~> K): FilenameExpr[K]
}
