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
  services: Map[String, ServiceRaw] = Map.empty,
  abilities: Map[String, RawContext] = Map.empty,
  rootServiceIds: Map[String, ValueRaw] = Map(),
  definitions: Map[String, NamedTypeToken[S]] = Map()
) {

  def setRootServiceId(name: String, id: ValueRaw): AbilitiesState[S] =
    copy(rootServiceIds = rootServiceIds.updated(name, id))

  def getDefaultServiceId(name: String): Option[ValueRaw] =
    services.get(name).flatMap(_.defaultId)

  def purgeArrows: Option[(NonEmptyList[(Name[S], ArrowType)], AbilitiesState[S])] =
    stack match {
      case sc :: tail =>
        NonEmptyList
          .fromList(sc.arrows.values.toList)
          .map(_ -> copy[S](sc.copy(arrows = Map.empty) :: tail))
      case _ => None
    }

  def isIdResolvedInPrevScope(name: String): Boolean =
    stack.tail.exists(_.isIdResolved(name))

  def getServiceId(name: String): Option[ValueRaw] =
    stack.collectFirstSome(_.getServiceId(name)) orElse
      rootServiceIds.get(name) orElse
      services.get(name).flatMap(_.defaultId)

  def getServiceRename(name: String): Option[String] =
    stack.collectFirstSome(_.getServiceRename(name)) orElse
      // Suppose that services without id
      // resolved in scope are not renamed
      rootServiceIds.get(name).as(name) orElse
      services.get(name).flatMap(_.defaultId).as(name)

}

object AbilitiesState {

  case class Frame[S[_]](
    token: Token[S],
    arrows: Map[String, (Name[S], ArrowType)] = Map(),
    services: Map[String, Frame.ServiceState] = Map()
  ) {

    def setServiceId(name: String, id: ValueRaw, rename: String): Frame[S] =
      copy(services =
        services.updated(
          name,
          Frame.ServiceState(id, rename)
        )
      )

    def isIdResolved(name: String): Boolean =
      services.get(name).isDefined

    def getServiceId(name: String): Option[ValueRaw] =
      services.get(name).map(_.id)

    def getServiceRename(name: String): Option[String] =
      services.get(name).map(_.rename)
  }

  object Frame {

    final case class ServiceState(
      id: ValueRaw,
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
      services = context.allServices,
      abilities = context.abilities // TODO is it the right way to collect abilities? Why?
    )
}
