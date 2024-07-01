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

package aqua.semantics.rules.types

import aqua.parser.lexer.*
import aqua.raw.RawContext
import aqua.raw.value.ValueRaw
import aqua.types.*

import cats.kernel.Monoid

case class TypesState[S[_]](
  fields: Map[String, (Name[S], Type)] = Map(),
  strict: Map[String, Type] = Map.empty,
  stack: List[TypesState.Frame[S]] = Nil
) {
  def isDefined(t: String): Boolean = strict.contains(t)

  def defineType(name: NamedTypeToken[S], `type`: Type): TypesState[S] =
    copy(
      strict = strict.updated(name.value, `type`)
    )

  def getType(name: String): Option[Type] =
    strict.get(name)
}

object TypesState {

  final case class TypeDefinition[S[_]](
    token: NamedTypeToken[S],
    `type`: Type
  )

  case class Frame[S[_]](
    token: ArrowTypeToken[S],
    arrowType: ArrowType,
    retVals: Option[List[ValueRaw]]
  )

  def init[S[_]](context: RawContext): TypesState[S] =
    TypesState(strict = context.allTypes)
}
