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

package aqua.semantics

import aqua.helpers.data.PName
import aqua.parser.lexer.Token
import aqua.semantics.rules.locations.LocationsAlgebra

import cats.data.State

package object header {

  /*
   NOTE: This extension glues locations algebra from the body semantics
         with the context that is used in the header semantics
   */
  extension [S[_], C](context: C)(using
    locations: LocationsAlgebra[S, State[C, *]]
  ) {

    def addOccurences(tokens: List[(PName, Token[S])]): C =
      locations.pointLocations(tokens).runS(context).value
  }
}
