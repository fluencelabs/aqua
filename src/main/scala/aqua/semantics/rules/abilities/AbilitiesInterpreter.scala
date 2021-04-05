package aqua.semantics.rules.abilities

import aqua.semantics.rules.{ReportError, StackInterpreter}
import aqua.parser.lexer.{Name, Token, Value}
import aqua.semantics.ArrowType
import cats.data.{NonEmptyList, NonEmptyMap, State}
import cats.~>
import cats.syntax.functor._
import monocle.Lens
import monocle.macros.GenLens

class AbilitiesInterpreter[F[_], X](implicit lens: Lens[X, AbilitiesState[F]], error: ReportError[F, X])
    extends StackInterpreter[F, X, AbilitiesState[F], AbilityStackFrame[F]](GenLens[AbilitiesState[F]](_.stack))
    with (AbilityOp[F, *] ~> State[X, *]) {

  private def getService(name: String): S[Option[NonEmptyMap[String, ArrowType]]] =
    getState.map(_.services.get(name))

  override def apply[A](fa: AbilityOp[F, A]): State[X, A] =
    (fa match {
      case bs: BeginScope[F] =>
        beginScope(AbilityStackFrame[F](bs.token))

      case EndScope() =>
        endScope

      case pa: PurgeArrows[F] =>
        getState.map(_.purgeArrows).flatMap {
          case Some((arrs, nextState)) => setState(nextState).as(Option[NonEmptyList[(Name[F], ArrowType)]](arrs))
          case _ =>
            report(pa.token, "Cannot purge arrows, no arrows provided")
              .as(Option.empty[NonEmptyList[(Name[F], ArrowType)]])
        }

      case ga: GetArrow[F] =>
        getService(ga.name.value).flatMap {
          case Some(arrows) =>
            arrows(ga.arrow.value)
              .fold(
                report(
                  ga.arrow,
                  s"Service found, but arrow is undefined, available: ${arrows.value.keys.toNonEmptyList.toList.mkString(", ")}"
                ).as(Option.empty[ArrowType])
              )(a => State.pure(Some(a)))
          case None =>
            report(ga.name, "Ability with this name is undefined").as(Option.empty[ArrowType])
        }

      case s: SetServiceId[F] =>
        getService(s.name.value).flatMap {
          case Some(_) =>
            mapStackHead(
              modify(st => st.copy(rootServiceIds = st.rootServiceIds.updated(s.name.value, s.id))).as(true)
            )(h => h.copy(serviceIds = h.serviceIds.updated(s.name.value, s.id)) -> true)

          case None =>
            report(s.name, "Service with this name is not registered, can't set its ID").as(false)
        }

      case s: GetServiceId[F] =>
        getState.flatMap(st =>
          st.stack.flatMap(_.serviceIds.get(s.name.value)).headOption orElse st.rootServiceIds.get(s.name.value) match {
            case None =>
              report(s.name, s"Service ID unresolved, use `${s.name.value} id` expression to set it")
                .as(Option.empty[Value[F]])

            case v => State.pure(v)
          }
        )

      case da: DefineArrow[F] =>
        mapStackHeadE(
          report(da.arrow, "No abilities definition scope is found").as(false)
        )(h =>
          h.arrows.get(da.arrow.value) match {
            case Some(_) => Left((da.arrow, "Arrow with this name was already defined above", false))
            case None =>
              Right(h.copy(arrows = h.arrows.updated(da.arrow.value, da.arrow -> da.`type`)) -> true)
          }
        )

      case ds: DefineService[F] =>
        getService(ds.name.value).flatMap {
          case Some(_) => report(ds.name, "Service with this name was already defined").as(false)
          case None => modify(s => s.copy(services = s.services.updated(ds.name.value, ds.arrows))).as(true)
        }

    }).asInstanceOf[State[X, A]]
}

case class AbilitiesState[F[_]](
  stack: List[AbilityStackFrame[F]] = Nil,
  services: Map[String, NonEmptyMap[String, ArrowType]] = Map.empty,
  rootServiceIds: Map[String, Value[F]] = Map.empty[String, Value[F]]
) {

  def purgeArrows: Option[(NonEmptyList[(Name[F], ArrowType)], AbilitiesState[F])] =
    stack match {
      case sc :: tail =>
        NonEmptyList.fromList(sc.arrows.values.toList).map(_ -> copy[F](sc.copy(arrows = Map.empty) :: tail))
      case _ => None
    }
}

case class AbilityStackFrame[F[_]](
  token: Token[F],
  arrows: Map[String, (Name[F], ArrowType)] = Map.empty[String, (Name[F], ArrowType)],
  serviceIds: Map[String, Value[F]] = Map.empty[String, Value[F]]
)
