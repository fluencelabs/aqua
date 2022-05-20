package aqua.semantics.rules.abilities

import aqua.parser.lexer.{Ability, Name, Token, ValueToken}
import aqua.raw.ServiceRaw
import aqua.raw.RawContext
import aqua.raw.value.ValueRaw
import aqua.semantics.{Levenshtein, TokenArrowInfo, TokenDef, TokenTypeInfo}
import aqua.semantics.rules.{abilities, ReportError, StackInterpreter}
import aqua.types.ArrowType
import cats.data.{NonEmptyList, NonEmptyMap, State}
import cats.syntax.functor.*
import cats.~>
import monocle.Lens
import monocle.macros.GenLens

class AbilitiesInterpreter[S[_], X](implicit
  lens: Lens[X, AbilitiesState[S]],
  error: ReportError[S, X]
) extends AbilitiesAlgebra[S, State[X, *]] {

  type SX[A] = State[X, A]

  val stackInt = new StackInterpreter[S, X, AbilitiesState[S], AbilitiesState.Frame[S]](
    GenLens[AbilitiesState[S]](_.stack)
  )

  import stackInt.{getState, mapStackHead, mapStackHeadE, modify, report, setState}

  override def defineArrow(arrow: Name[S], `type`: ArrowType): SX[Boolean] =
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

  override def purgeArrows(token: Token[S]): SX[Option[NonEmptyList[(Name[S], ArrowType)]]] =
    getState.map(_.purgeArrows).flatMap {
      case Some((arrs, nextState)) =>
        setState(nextState).as(Option[NonEmptyList[(Name[S], ArrowType)]](arrs))
      case _ =>
        report(token, "Cannot purge arrows, no arrows provided")
          .as(Option.empty[NonEmptyList[(Name[S], ArrowType)]])
    }

  override def defineService(
    name: Ability[S],
    arrows: NonEmptyMap[String, (Name[S], ArrowType)],
    defaultId: Option[ValueRaw]
  ): SX[Boolean] =
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
              .updated(name.value, ServiceRaw(name.value, arrows.map(_._2), defaultId)),
            definitions =
              s.definitions.updated(name.value, (name, arrows.toSortedMap.values.toList))
          )
        ).as(true)
    }

  def addServiceArrowLocation(name: Ability[S], arrow: Name[S]): SX[Unit] = {
    getState.flatMap { st =>
      st.definitions.get(name.value) match {
        case Some((ab, arrows)) =>
          modify(st =>
            st.copy(locations =
              st.locations ++ (
                (name, TokenDef(Some(ab))) :: (
                  arrow,
                  TokenDef(
                    arrows.find(_._1.value == arrow.value).map(_._1)
                  )
                ) :: Nil
              )
            )
          )
        case None =>
          State.pure(())
      }
    }
  }

  override def getArrow(name: Ability[S], arrow: Name[S]): SX[Option[ArrowType]] =
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
          )(a => addServiceArrowLocation(name, arrow).as(Some(a)))
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
              ) { fn =>
                addServiceArrowLocation(name, arrow).as(Some(fn.arrow.`type`))
              }
          case None =>
            report(name, "Ability with this name is undefined").as(Option.empty[ArrowType])
        }
    }

  override def setServiceId(name: Ability[S], id: ValueToken[S], vm: ValueRaw): SX[Boolean] =
    getService(name.value).flatMap {
      case Some(_) =>
        mapStackHead(
          modify(st => st.copy(rootServiceIds = st.rootServiceIds.updated(name.value, id -> vm)))
            .as(true)
        )(h => h.copy(serviceIds = h.serviceIds.updated(name.value, id -> vm)) -> true)

      case None =>
        report(name, "Service with this name is not registered, can't set its ID").as(false)
    }

  override def getServiceId(name: Ability[S]): SX[Either[Boolean, ValueRaw]] =
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
                .as(Left[Boolean, ValueRaw](false))

            case Some(v) => State.pure(Right(v))
          }
        )
      case None =>
        getAbility(name.value).flatMap {
          case Some(_) => State.pure(Left[Boolean, ValueRaw](true))
          case None =>
            report(name, "Ability with this name is undefined").as(
              Left[Boolean, ValueRaw](false)
            )
        }
    }

  override def beginScope(token: Token[S]): SX[Unit] =
    stackInt.beginScope(AbilitiesState.Frame[S](token))

  override def endScope(): SX[Unit] = stackInt.endScope

  private def getService(name: String): SX[Option[ServiceRaw]] =
    getState.map(_.services.get(name))

  private def getAbility(name: String): SX[Option[RawContext]] =
    getState.map(_.abilities.get(name))
}
