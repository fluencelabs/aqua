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

import aqua.parser.lexer.Token
import aqua.semantics.rules.locations.{DefinitionInfo, LocationsAlgebra, LocationsState}
import aqua.types.AbilityType

import cats.data.State
import monocle.Lens
import scribe.Logging

class LocationsInterpreter[S[_], X](using
  lens: Lens[X, LocationsState[S]]
) extends LocationsAlgebra[S, State[X, *]] with Logging {

  type SX[A] = State[X, A]

  override def addDefinition(definition: DefinitionInfo[S]): State[X, Unit] =
    definition.`type` match {
      // case where ability is an {Argument} in a function
      case t: AbilityType if definition.name == t.name =>
        pointLocation(definition.name, definition.token)
      case _ => modify { st => st.addDefinition(definition) }
    }

  override def addDefinitionWithFields(
    definition: DefinitionInfo[S],
    fields: List[DefinitionInfo[S]]
  ): State[X, Unit] = {
    val allTokens =
      definition +: fields.map { fieldDef =>
        fieldDef.copy(name = AbilityType.fullName(definition.name, fieldDef.name))
      }
    modify { st =>
      st.addDefinitions(allTokens)
    }
  }

  def pointFieldLocation(typeName: String, fieldName: String, token: Token[S]): State[X, Unit] =
    pointLocation(AbilityType.fullName(typeName, fieldName), token)

  def pointTokenWithFieldLocation(
    typeName: String,
    typeToken: Token[S],
    fieldName: String,
    token: Token[S]
  ): State[X, Unit] = {
    for {
      _ <- pointLocation(typeName, typeToken)
      _ <- pointLocation(AbilityType.fullName(typeName, fieldName), token)
    } yield {}
  }

  override def pointLocation(name: String, token: Token[S]): State[X, Unit] = {
    modify { st =>
      st.addLocation(name, token)
    }
  }

  def pointLocations(locations: List[(String, Token[S])]): State[X, Unit] =
    modify { st =>
      st.addLocations(locations)
    }

  private def modify(f: LocationsState[S] => LocationsState[S]): SX[Unit] =
    State.modify(lens.modify(f))
}
