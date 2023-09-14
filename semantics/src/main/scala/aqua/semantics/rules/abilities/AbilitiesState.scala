package aqua.semantics.rules.abilities

import aqua.raw.{RawContext, ServiceRaw}
import aqua.raw.value.ValueRaw
import aqua.parser.lexer.{Name, NamedTypeToken, Token, ValueToken}
import aqua.types.ArrowType

import cats.Monoid
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.data.NonEmptyList
import aqua.parser.lexer.Token.name

case class AbilitiesState[S[_]](
  stack: List[AbilitiesState.Frame[S]] = Nil,
  services: Set[String] = Set.empty,
  abilities: Map[String, RawContext] = Map.empty,
  rootServiceIds: Map[String, ValueRaw] = Map(),
  definitions: Map[String, NamedTypeToken[S]] = Map()
) {

  def setRootServiceId(name: String, id: ValueRaw): AbilitiesState[S] =
    copy(rootServiceIds = rootServiceIds.updated(name, id))

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

  given [S[_]]: Monoid[AbilitiesState[S]] with {
    override def empty: AbilitiesState[S] = AbilitiesState()

    override def combine(x: AbilitiesState[S], y: AbilitiesState[S]): AbilitiesState[S] =
      AbilitiesState(
        Nil,
        x.services ++ y.services,
        x.abilities ++ y.abilities,
        x.rootServiceIds ++ y.rootServiceIds,
        x.definitions ++ y.definitions
      )
  }

  def init[S[_]](context: RawContext): AbilitiesState[S] =
    AbilitiesState(
      services = context.allServices.keySet,
      rootServiceIds = context.allServices.flatMap { case (name, service) =>
        service.defaultId.map(name -> _)
      },
      abilities = context.abilities // TODO is it the right way to collect abilities? Why?
    )
}
