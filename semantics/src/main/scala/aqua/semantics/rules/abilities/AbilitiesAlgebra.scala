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

package aqua.semantics.rules.abilities

import aqua.parser.lexer.{Name, NamedTypeToken, Token, ValueToken}
import aqua.raw.value.ValueRaw
import aqua.types.{ArrowType, ServiceType}

import cats.InjectK
import cats.data.{NonEmptyList, NonEmptyMap}

trait AbilitiesAlgebra[S[_], Alg[_]] {

  def defineService(
    name: NamedTypeToken[S],
    arrowDefs: NonEmptyMap[String, Name[S]],
    defaultId: Option[ValueRaw]
  ): Alg[Boolean]

  def isDefinedAbility(name: NamedTypeToken[S]): Alg[Boolean]

  def getArrow(name: NamedTypeToken[S], arrow: Name[S]): Alg[Option[ArrowType]]

  def renameService(name: NamedTypeToken[S]): Alg[Option[String]]

  def getServiceRename(name: NamedTypeToken[S]): Alg[Option[String]]

  def beginScope(token: Token[S]): Alg[Unit]

  def endScope(): Alg[Unit]

}
