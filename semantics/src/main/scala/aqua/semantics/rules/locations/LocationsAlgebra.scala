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

import aqua.helpers.data.{PName, SName}
import aqua.parser.lexer.Token
import aqua.types.Type

trait LocationsAlgebra[S[_], Alg[_]] {
  def addDefinition(definition: DefinitionInfo[S]): Alg[Unit]

  def addDefinitionWithFields(
    definition: DefinitionInfo[S],
    fields: List[DefinitionInfo[S]]
  ): Alg[Unit]

  def pointTokenWithFieldLocation(
    typeName: PName,
    typeToken: Token[S],
    fieldName: SName,
    token: Token[S]
  ): Alg[Unit]

  def pointFieldLocation(
    typeName: PName,
    fieldName: SName,
    token: Token[S]
  ): Alg[Unit]

  def pointLocation(name: PName, token: Token[S]): Alg[Unit]
  def pointLocations(locations: List[(PName, Token[S])]): Alg[Unit]
}
