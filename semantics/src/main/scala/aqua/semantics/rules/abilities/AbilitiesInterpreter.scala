package aqua.semantics.rules.abilities

import aqua.model.{ServiceModel, ValueModel}
import aqua.parser.lexer.Name
import aqua.semantics.rules.{ReportError, StackInterpreter, ValuesAlgebra}
import aqua.types.{ArrowType, ScalarType}
import cats.data.{NonEmptyList, State}
import cats.syntax.functor._
import cats.~>
import monocle.Lens
import monocle.macros.GenLens

class AbilitiesInterpreter[F[_], X](implicit
  lens: Lens[X, AbilitiesState[F]],
  error: ReportError[F, X]
) extends StackInterpreter[F, X, AbilitiesState[F], AbilitiesState.Frame[F]](
      GenLens[AbilitiesState[F]](_.stack)
    ) with (AbilityOp[F, *] ~> State[X, *]) {

  private def getService(name: String): S[Option[ServiceModel]] =
    getState.map(_.services.get(name))

  override def apply[A](fa: AbilityOp[F, A]): State[X, A] =
    (fa match {
      case bs: BeginScope[F] =>
        beginScope(AbilitiesState.Frame[F](bs.token))

      case EndScope() =>
        endScope

      case pa: PurgeArrows[F] =>
        getState.map(_.purgeArrows).flatMap {
          case Some((arrs, nextState)) =>
            setState(nextState).as(Option[NonEmptyList[(Name[F], ArrowType)]](arrs))
          case _ =>
            report(pa.token, "Cannot purge arrows, no arrows provided")
              .as(Option.empty[NonEmptyList[(Name[F], ArrowType)]])
        }

      case ga: GetArrow[F] =>
        getService(ga.name.value).map(_.map(_.arrows)).flatMap {
          case Some(arrows) =>
            arrows(ga.arrow.value)
              .fold(
                report(
                  ga.arrow,
                  s"Service found, but arrow is undefined, available: ${arrows.value.keys.toNonEmptyList.toList
                    .mkString(", ")}"
                ).as(Option.empty[ArrowType])
              )(a => State.pure(Some(a)))
          case None =>
            report(ga.name, "Ability with this name is undefined").as(Option.empty[ArrowType])
        }

      case s: SetServiceId[F] =>
        getService(s.name.value).flatMap {
          case Some(_) =>
            val vm = ValuesAlgebra.valueToModel(s.id, ScalarType.string)
            mapStackHead(
              modify(st =>
                st.copy(rootServiceIds =
                  st.rootServiceIds
                    .updated(s.name.value, vm)
                )
              )
                .as(true)
            )(h => h.copy(serviceIds = h.serviceIds.updated(s.name.value, vm)) -> true)

          case None =>
            report(s.name, "Service with this name is not registered, can't set its ID").as(false)
        }

      case s: GetServiceId[F] =>
        getState.flatMap(st =>
          st.stack.flatMap(_.serviceIds.get(s.name.value)).headOption orElse st.rootServiceIds.get(
            s.name.value
          ) match {
            case None =>
              report(
                s.name,
                s"Service ID unresolved, use `${s.name.value} id` expression to set it"
              )
                .as(Option.empty[ValueModel])

            case v => State.pure(v)
          }
        )

      case da: DefineArrow[F] =>
        mapStackHeadE(
          report(da.arrow, "No abilities definition scope is found").as(false)
        )(h =>
          h.arrows.get(da.arrow.value) match {
            case Some(_) =>
              Left((da.arrow, "Arrow with this name was already defined above", false))
            case None =>
              Right(
                h.copy(arrows = h.arrows.updated(da.arrow.value, da.arrow -> da.`type`)) -> true
              )
          }
        )

      case ds: DefineService[F] =>
        getService(ds.name.value).flatMap {
          case Some(_) =>
            getState.map(_.definitions.get(ds.name.value).exists(_ == ds.name)).flatMap {
              case true => State.pure(false)
              case false => report(ds.name, "Service with this name was already defined").as(false)

            }
          case None =>
            modify(s =>
              s.copy(
                services = s.services
                  .updated(ds.name.value, ServiceModel(ds.name.value, ds.arrows, ds.v)),
                definitions = s.definitions.updated(ds.name.value, ds.name)
              )
            ).as(true)
        }

    }).asInstanceOf[State[X, A]]
}
