package aqua.semantics.rules.abilities

import aqua.raw.{RawContext, ServiceRaw}
import aqua.raw.value.ValueRaw
import aqua.parser.lexer.{Name, NamedTypeToken, Token, ValueToken}
import aqua.types.ArrowType
import cats.Monoid
import cats.data.NonEmptyList

case class AbilitiesState[S[_]](
  stack: List[AbilitiesState.Frame[S]] = Nil,
  services: Map[String, ServiceRaw] = Map.empty,
  abilities: Map[String, RawContext] = Map.empty,
  rootServiceIds: Map[String, (ValueToken[S], ValueRaw)] =
    Map.empty[String, (ValueToken[S], ValueRaw)],
  definitions: Map[String, NamedTypeToken[S]] = Map.empty[String, NamedTypeToken[S]]
) {

  def purgeArrows: Option[(NonEmptyList[(Name[S], ArrowType)], AbilitiesState[S])] =
    stack match {
      case sc :: tail =>
        NonEmptyList
          .fromList(sc.arrows.values.toList)
          .map(_ -> copy[S](sc.copy(arrows = Map.empty) :: tail))
      case _ => None
    }
}

object AbilitiesState {

  case class Frame[S[_]](
    token: Token[S],
    arrows: Map[String, (Name[S], ArrowType)] = Map.empty[String, (Name[S], ArrowType)],
    serviceIds: Map[String, (ValueToken[S], ValueRaw)] =
      Map.empty[String, (ValueToken[S], ValueRaw)]
  )

  implicit def abilitiesStateMonoid[S[_]]: Monoid[AbilitiesState[S]] =
    new Monoid[AbilitiesState[S]] {
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
