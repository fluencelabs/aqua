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
import cats.data.State

class DummyLocationsInterpreter[S[_], X] extends LocationsAlgebra[S, State[X, *]] {

  def addDefinition(definition: DefinitionInfo[S]): State[X, Unit] = State.pure(())

  def addDefinitionWithFields(
    definition: DefinitionInfo[S],
    fields: List[DefinitionInfo[S]]
  ): State[X, Unit] = State.pure(())

  def pointFieldLocation(typeName: String, fieldName: String, token: Token[S]): State[X, Unit] =
    State.pure(())

  def pointTokenWithFieldLocation(
    typeName: String,
    typeToken: Token[S],
    fieldName: String,
    token: Token[S]
  ): State[X, Unit] = State.pure(())
  override def pointLocation(name: String, token: Token[S]): State[X, Unit] = State.pure(())
  override def pointLocations(locations: List[(String, Token[S])]): State[X, Unit] = State.pure(())

  def beginScope(): State[X, Unit] = State.pure(())

  def endScope(): State[X, Unit] = State.pure(())
}
