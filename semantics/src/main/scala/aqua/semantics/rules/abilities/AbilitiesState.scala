package aqua.semantics.rules.abilities

import aqua.model.{AquaContext, ServiceModel, ValueModel}
import aqua.parser.lexer.{Ability, Name, Token, Value}
import aqua.types.ArrowType
import cats.Monoid
import cats.data.NonEmptyList

case class AbilitiesState[S[_]](
  stack: List[AbilitiesState.Frame[S]] = Nil,
  services: Map[String, ServiceModel] = Map.empty,
  abilities: Map[String, AquaContext] = Map.empty,
  rootServiceIds: Map[String, (Value[S], ValueModel)] = Map.empty[String, (Value[S], ValueModel)],
  definitions: Map[String, Ability[S]] = Map.empty[String, Ability[S]]
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
    serviceIds: Map[String, (Value[S], ValueModel)] = Map.empty[String, (Value[S], ValueModel)]
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

  def init[S[_]](context: AquaContext): AbilitiesState[S] =
    AbilitiesState(
      services = context.allServices(),
      abilities = context.abilities // TODO is it the right way to collect abilities? Why?
    )
}
