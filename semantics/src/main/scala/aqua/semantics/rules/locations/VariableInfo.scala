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

package aqua.semantics.rules.locations

import aqua.parser.lexer.Token
import aqua.types.Type

case class DefinitionInfo[S[_]](name: String, token: Token[S], `type`: Type)
case class TokenLocation[S[_]](usage: Token[S], definition: Token[S])

case class VariableInfo[S[_]](definition: DefinitionInfo[S], occurrences: List[Token[S]] = Nil) {
  def allLocations: List[TokenLocation[S]] = occurrences.map(o => TokenLocation(o, definition.token))
}
