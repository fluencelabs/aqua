package aqua.semantics.rules.abilities

import aqua.model.{AquaContext, ServiceModel, ValueModel}
import aqua.parser.lexer.{Ability, Name, Token, Value}
import aqua.semantics.Levenshtein
import aqua.semantics.rules.{ReportError, StackInterpreter, abilities}
import aqua.types.ArrowType
import cats.data.{NonEmptyList, NonEmptyMap, State}
import cats.syntax.functor.*
import cats.~>
import monocle.Lens
import monocle.macros.GenLens

class AbilitiesInterpreter[F[_], X](implicit
  lens: Lens[X, AbilitiesState[F]],
  error: ReportError[F, X]
) extends AbilitiesAlgebra[F, State[X, *]] {

  type SA[A] = State[X, A]

  val stackInt = new StackInterpreter[F, X, AbilitiesState[F], AbilitiesState.Frame[F]](
    GenLens[AbilitiesState[F]](_.stack)
  )

  import stackInt.{report, modify, mapStackHead, getState, mapStackHeadE, setState}

  override def defineArrow(arrow: Name[F], `type`: ArrowType): SA[Boolean] =
    mapStackHeadE(
      report(arrow, "No abilities definition scope is found").as(false)
    )(h =>
      h.arrows.get(arrow.value) match {
        case Some(_) =>
          Left((arrow, "Arrow with this name was already defined above", false))
        case None =>
          Right(
            h.copy(arrows = h.arrows.updated(arrow.value, arrow -> `type`)) -> true
          )
      }
    )

  override def purgeArrows(token: Token[F]): SA[Option[NonEmptyList[(Name[F], ArrowType)]]] =
    getState.map(_.purgeArrows).flatMap {
      case Some((arrs, nextState)) =>
        setState(nextState).as(Option[NonEmptyList[(Name[F], ArrowType)]](arrs))
      case _ =>
        report(token, "Cannot purge arrows, no arrows provided")
          .as(Option.empty[NonEmptyList[(Name[F], ArrowType)]])
    }

  override def defineService(
    name: Ability[F],
    arrows: NonEmptyMap[String, ArrowType],
    defaultId: Option[ValueModel]
  ): SA[Boolean] =
    getService(name.value).flatMap {
      case Some(_) =>
        getState.map(_.definitions.get(name.value).exists(_ == name)).flatMap {
          case true => State.pure(false)
          case false => report(name, "Service with this name was already defined").as(false)

        }
      case None =>
        modify(s =>
          s.copy(
            services = s.services
              .updated(name.value, ServiceModel(name.value, arrows, defaultId)),
            definitions = s.definitions.updated(name.value, name)
          )
        ).as(true)
    }

  override def getArrow(name: Ability[F], arrow: Name[F]): SA[Option[ArrowType]] =
    getService(name.value).map(_.map(_.arrows)).flatMap {
      case Some(arrows) =>
        arrows(arrow.value)
          .fold(
            report(
              arrow,
              Levenshtein.genMessage(
                s"Service is found, but arrow '${arrow.value}' isn't found in scope",
                arrow.value,
                arrows.value.keys.toNonEmptyList.toList
              )
            ).as(Option.empty[ArrowType])
          )(a => State.pure(Some(a)))
      case None =>
        getAbility(name.value).flatMap {
          case Some(abCtx) =>
            abCtx.funcs
              .get(arrow.value)
              .fold(
                report(
                  arrow,
                  Levenshtein.genMessage(
                    s"Ability is found, but arrow '${arrow.value}' isn't found in scope",
                    arrow.value,
                    abCtx.funcs.keys.toList
                  )
                ).as(Option.empty[ArrowType])
              )(fn => State.pure(Some(fn.arrowType)))
          case None =>
            report(name, "Ability with this name is undefined").as(Option.empty[ArrowType])
        }
    }

  override def setServiceId(name: Ability[F], id: Value[F], vm: ValueModel): SA[Boolean] =
    getService(name.value).flatMap {
      case Some(_) =>
        mapStackHead(
          modify(st =>
            st.copy(rootServiceIds = st.rootServiceIds.updated(name.value, id -> vm))
          )
            .as(true)
        )(h => h.copy(serviceIds = h.serviceIds.updated(name.value, id -> vm)) -> true)

      case None =>
        report(name, "Service with this name is not registered, can't set its ID").as(false)
    }

  override def getServiceId(name: Ability[F]): SA[Either[Boolean, ValueModel]] =
    getService(name.value).flatMap {
      case Some(_) =>
        getState.flatMap(st =>
          st.stack
            .flatMap(_.serviceIds.get(name.value).map(_._2))
            .headOption orElse st.rootServiceIds
            .get(
              name.value
            )
            .map(_._2) orElse st.services.get(name.value).flatMap(_.defaultId) match {
            case None =>
              report(
                name,
                s"Service ID unresolved, use `${name.value} id` expression to set it"
              )
                .as(Left[Boolean, ValueModel](false))

            case Some(v) => State.pure(Right(v))
          }
        )
      case None =>
        getAbility(name.value).flatMap {
          case Some(_) => State.pure(Left[Boolean, ValueModel](true))
          case None =>
            report(name, "Ability with this name is undefined").as(
              Left[Boolean, ValueModel](false)
            )
        }
    }

  override def beginScope(token: Token[F]): SA[Unit] =
    stackInt.beginScope(AbilitiesState.Frame[F](token))

  override def endScope(): SA[Unit] = stackInt.endScope

  private def getService(name: String): SA[Option[ServiceModel]] =
    getState.map(_.services.get(name))

  private def getAbility(name: String): SA[Option[AquaContext]] =
    getState.map(_.abilities.get(name))
}
