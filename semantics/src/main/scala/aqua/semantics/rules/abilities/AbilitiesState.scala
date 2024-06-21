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

import aqua.parser.lexer.Token.name
import aqua.parser.lexer.{Name, NamedTypeToken, Token, ValueToken}
import aqua.raw.value.ValueRaw
import aqua.raw.{RawContext, ServiceRaw}
import aqua.types.ArrowType

import cats.Monoid
import cats.data.NonEmptyList
import cats.syntax.foldable.*
import cats.syntax.functor.*

case class AbilitiesState[S[_]](
  stack: List[AbilitiesState.Frame[S]] = Nil,
  services: Set[String] = Set.empty,
  abilities: Map[String, RawContext] = Map.empty,
  rootServiceIds: Map[String, ValueRaw] = Map(),
  definitions: Map[String, NamedTypeToken[S]] = Map()
) {

  def defineService(name: NamedTypeToken[S], defaultId: Option[ValueRaw]): AbilitiesState[S] =
    copy(
      services = services + name.value,
      definitions = definitions.updated(name.value, name),
      rootServiceIds = rootServiceIds ++ defaultId.map(name.value -> _)
    )

  def getServiceRename(name: String): Option[String] =
    stack.collectFirstSome(_.getServiceRename(name)) orElse
      // Suppose that services without id
      // resolved in scope are not renamed
      rootServiceIds.get(name).as(name)

}

object AbilitiesState {

  case class Frame[S[_]](
    token: Token[S],
    services: Map[String, Frame.ServiceState] = Map()
  ) {

    def setServiceRename(name: String, rename: String): Frame[S] =
      copy(services =
        services.updated(
          name,
          Frame.ServiceState(rename)
        )
      )

    def getServiceRename(name: String): Option[String] =
      services.get(name).map(_.rename)
  }

  object Frame {

    final case class ServiceState(
      rename: String
    )
  }

  def init[S[_]](context: RawContext): AbilitiesState[S] =
    AbilitiesState(
      services = context.allServices.keySet,
      rootServiceIds = context.allServices.flatMap { case (name, service) =>
        service.defaultId.map(name -> _)
      },
      abilities = context.allAbilities
    )
}
