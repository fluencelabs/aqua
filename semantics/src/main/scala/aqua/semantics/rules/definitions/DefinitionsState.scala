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

package aqua.semantics.rules.definitions

import aqua.parser.lexer.{Name, Token}
import aqua.types.Type

import DefinitionsState.Def

case class DefinitionsState[S[_]](
  definitions: Map[String, Def[S]] = Map.empty[String, Def[S]]
)

object DefinitionsState {

  final case class Def[S[_]](
    name: Name[S],
    `type`: Type
  )
}
