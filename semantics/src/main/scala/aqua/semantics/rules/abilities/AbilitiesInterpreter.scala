package aqua.semantics.rules.abilities

import aqua.parser.lexer.{Name, NamedTypeToken, Token, ValueToken}
import aqua.raw.value.ValueRaw
import aqua.raw.{RawContext, ServiceRaw}
import aqua.semantics.Levenshtein
import aqua.semantics.rules.errors.ReportErrors
import aqua.semantics.rules.mangler.ManglerAlgebra
import aqua.semantics.rules.locations.LocationsAlgebra
import aqua.semantics.rules.{abilities, StackInterpreter}
import aqua.types.{ArrowType, ServiceType}

import cats.data.{NonEmptyMap, State}
import cats.syntax.functor.*
import cats.syntax.apply.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import cats.syntax.applicative.*
import cats.syntax.option.*
import monocle.Lens
import monocle.macros.GenLens

class AbilitiesInterpreter[S[_], X](using
  lens: Lens[X, AbilitiesState[S]],
  error: ReportErrors[S, X],
  mangler: ManglerAlgebra[State[X, *]],
  locations: LocationsAlgebra[S, State[X, *]]
) extends AbilitiesAlgebra[S, State[X, *]] {

  type SX[A] = State[X, A]

  private val stackInt = new StackInterpreter[S, X, AbilitiesState[S], AbilitiesState.Frame[S]](
    GenLens[AbilitiesState[S]](_.stack)
  )

  import stackInt.{getState, mapStackHead, mapStackHeadM, modify, report}

  override def defineService(
    name: NamedTypeToken[S],
    arrowDefs: NonEmptyMap[String, Name[S]],
    serviceType: ServiceType,
    defaultId: Option[ValueRaw]
  ): SX[Boolean] =
    getService(name.value).flatMap {
      case Some(_) =>
        getState
          .map(_.definitions.get(name.value).exists(_ == name))
          .flatMap(exists =>
            report(
              name,
              "Service with this name was already defined"
            ).whenA(!exists)
          )
          .as(false)
      case None =>
        for {
          _ <- modify(s =>
            s.copy(
              services = s.services.updated(
                name.value,
                ServiceRaw(name.value, serviceType, defaultId)
              ),
              definitions = s.definitions.updated(name.value, name)
            )
          )
          _ <- locations.addTokenWithFields(
            name.value,
            name,
            arrowDefs.toNel.toList
          )
        } yield true
    }

  // adds location from token to its definition
  private def addServiceArrowLocation(name: NamedTypeToken[S], arrow: Name[S]): SX[Unit] = {
    locations.pointTokenWithFieldLocation(name.value, name, arrow.value, arrow)
  }

  override def getArrow(name: NamedTypeToken[S], arrow: Name[S]): SX[Option[ArrowType]] =
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
            ).as(none)
          ) { fn =>
            // TODO: add name and arrow separately
            // TODO: find tokens somewhere
            addServiceArrowLocation(name, arrow).as(fn.arrow.`type`.some)
          }
      case None =>
        report(name, "Ability with this name is undefined").as(none)
    }

  override def setServiceId(name: NamedTypeToken[S], id: ValueRaw): SX[Option[String]] =
    getService(name.value).flatMap {
      case Some(_) =>
        mapStackHeadM(
          modify(_.setRootServiceId(name.value, id)).as(name.value)
        )(h =>
          mangler
            .rename(name.value)
            .map(newName => h.setServiceId(name.value, id, newName) -> newName)
        ).map(_.some)
      case None =>
        report(name, "Service with this name is not registered, can't set its ID").as(none)
    }

  override def getServiceRename(name: NamedTypeToken[S]): State[X, Option[String]] =
    (
      getService(name.value),
      getState.map(_.getServiceRename(name.value))
    ).flatMapN {
      case (Some(_), Some(rename)) => rename.some.pure
      case (None, _) => report(name, "Service with this name is undefined").as(none)
      case (_, None) => report(name, "Service ID is undefined").as(none)
    }

  override def getDefaultServiceIdIfUnresolvedInPrevScope(
    name: String
  ): State[X, Option[ValueRaw]] =
    getState.map(st =>
      st.getDefaultServiceId(name)
        .filterNot(_ => st.isIdResolvedInPrevScope(name))
    )

  override def beginScope(token: Token[S]): SX[Unit] =
    stackInt.beginScope(AbilitiesState.Frame[S](token))

  override def endScope(): SX[Unit] = stackInt.endScope

  private def getService(name: String): SX[Option[ServiceRaw]] =
    getState.map(_.services.get(name))

  private def getAbility(name: String): SX[Option[RawContext]] =
    getState.map(_.abilities.get(name))
}
