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

import aqua.parser.lexer.{Name, NamedTypeToken, Token}
import aqua.types.{ArrowType, Type}

import cats.data.{NonEmptyList, NonEmptyMap}

// Collect and purge arrows/values from structures, services, etc
trait DefinitionsAlgebra[S[_], Alg[_]] {
  def defineDef(name: Name[S], `type`: Type): Alg[Boolean]

  def purgeDefs(): Alg[Map[String, DefinitionsState.Def[S]]]

  def defineArrow(arrow: Name[S], `type`: ArrowType): Alg[Boolean]

  def purgeArrows(token: Token[S]): Alg[Option[NonEmptyList[(Name[S], ArrowType)]]]
}
