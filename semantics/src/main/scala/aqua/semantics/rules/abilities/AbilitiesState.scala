package aqua.semantics.rules.abilities

import aqua.model.{ServiceModel, ValueModel}
import aqua.parser.lexer.{Ability, Name, Token}
import aqua.types.ArrowType
import cats.Monoid
import cats.data.NonEmptyList

case class AbilitiesState[F[_]](
  stack: List[AbilitiesState.Frame[F]] = Nil,
  // global
  services: Map[String, ServiceModel] = Map.empty,
  // global service ids
  rootServiceIds: Map[String, ValueModel] = Map.empty[String, ValueModel],
  definitions: Map[String, Ability[F]] = Map.empty[String, Ability[F]]
) {

  def purgeArrows: Option[(NonEmptyList[(Name[F], ArrowType)], AbilitiesState[F])] =
    stack match {
      case sc :: tail =>
        NonEmptyList
          .fromList(sc.arrows.values.toList)
          .map(_ -> copy[F](sc.copy(arrows = Map.empty) :: tail))
      case _ => None
    }
}

object AbilitiesState {

  case class Frame[F[_]](
    token: Token[F],
    arrows: Map[String, (Name[F], ArrowType)] = Map.empty[String, (Name[F], ArrowType)],
    serviceIds: Map[String, ValueModel] = Map.empty[String, ValueModel]
  )

  implicit def abilitiesStateMonoid[F[_]]: Monoid[AbilitiesState[F]] =
    new Monoid[AbilitiesState[F]] {
      override def empty: AbilitiesState[F] = AbilitiesState()

      override def combine(x: AbilitiesState[F], y: AbilitiesState[F]): AbilitiesState[F] =
        AbilitiesState(
          Nil,
          x.services ++ y.services,
          x.rootServiceIds ++ y.rootServiceIds,
          x.definitions ++ y.definitions
        )
    }
}
