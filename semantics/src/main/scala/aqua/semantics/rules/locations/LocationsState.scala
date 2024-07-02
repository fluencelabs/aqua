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

import aqua.helpers.data.PName
import aqua.helpers.syntax.list.*
import aqua.parser.lexer.Token

import cats.kernel.Monoid
import cats.syntax.semigroup.*
import monocle.Lens
import monocle.macros.GenLens
import scribe.Logging

case class LocationsState[S[_]](
  variables: Variables[S] = Variables[S]()
) extends Logging {

  lazy val locations: List[TokenLocation[S]] = variables.locations

  def addDefinitions(newDefinitions: List[DefinitionInfo[S]]): LocationsState[S] =
    modifyVariables(_.addDefinitions(newDefinitions))

  def addDefinition(newDef: DefinitionInfo[S]): LocationsState[S] =
    addDefinitions(List(newDef))

  def addLocation(
    name: PName,
    token: Token[S]
  ): LocationsState[S] =
    modifyVariables(_.addOccurence(name, token))

  def addLocations(
    locations: List[(PName, Token[S])]
  ): LocationsState[S] =
    locations.foldLeft(this) { case (st, (name, token)) =>
      st.addLocation(name, token)
    }

  private def modifyVariables(f: Variables[S] => Variables[S]): LocationsState[S] =
    LocationsState.variablesLens[S].modify(f)(this)
}

object LocationsState {

  def variablesLens[S[_]]: Lens[LocationsState[S], Variables[S]] =
    GenLens[LocationsState[S]](_.variables)

  given [S[_]]: Monoid[LocationsState[S]] with {
    override def empty: LocationsState[S] = LocationsState[S]()

    override def combine(x: LocationsState[S], y: LocationsState[S]): LocationsState[S] =
      LocationsState(
        variables = x.variables.combine(y.variables)
      )
  }
}
