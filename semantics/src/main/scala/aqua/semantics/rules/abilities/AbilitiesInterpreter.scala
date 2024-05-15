package aqua.semantics.rules.abilities

import aqua.parser.lexer.{Name, NamedTypeToken, Token}
import aqua.raw.RawContext
import aqua.raw.value.ValueRaw
import aqua.semantics.Levenshtein
import aqua.semantics.rules.locations.LocationsAlgebra
import aqua.semantics.rules.mangler.ManglerAlgebra
import aqua.semantics.rules.report.ReportAlgebra
import aqua.semantics.rules.{StackInterpreter, abilities}
import aqua.types.ArrowType

import cats.data.*
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import monocle.Lens
import monocle.macros.GenLens

class AbilitiesInterpreter[S[_], X](using
  lens: Lens[X, AbilitiesState[S]],
  report: ReportAlgebra[S, State[X, *]],
  mangler: ManglerAlgebra[State[X, *]],
  locations: LocationsAlgebra[S, State[X, *]]
) extends AbilitiesAlgebra[S, State[X, *]] {

  type SX[A] = State[X, A]

  private val stackInt = new StackInterpreter[S, X, AbilitiesState[S], AbilitiesState.Frame[S]](
    GenLens[AbilitiesState[S]](_.stack)
  )

  import stackInt.*

  override def defineService(
    name: NamedTypeToken[S],
    arrowDefs: NonEmptyMap[String, Name[S]],
    defaultId: Option[ValueRaw]
  ): SX[Boolean] =
    serviceExists(name.value).flatMap {
      case true =>
        getState
          .map(_.definitions.get(name.value).exists(_ == name))
          .flatMap(exists =>
            report
              .error(
                name,
                "Service with this name was already defined"
              )
              .whenA(!exists)
          )
          .as(false)
      case false =>
        for {
          _ <- modify(_.defineService(name, defaultId))
        } yield true
    }

  // adds location from token to its definition
  private def addServiceArrowLocation(name: NamedTypeToken[S], arrow: Name[S]): SX[Unit] =
    locations.pointTokenWithFieldLocation(name.pathName, name, arrow.simpleName, arrow)

  override def isDefinedAbility(name: NamedTypeToken[S]): State[X, Boolean] =
    OptionT(getState.map(_.abilities.get(name.value))).semiflatTap { _ =>
      locations.pointLocation(name.pathName, name)
    }.isDefined

  override def getArrow(name: NamedTypeToken[S], arrow: Name[S]): SX[Option[ArrowType]] =
    getAbility(name.value).flatMap {
      case Some(abCtx) =>
        abCtx.funcs
          .get(arrow.simpleName)
          .fold(
            report
              .error(
                arrow,
                Levenshtein.genMessage(
                  s"Ability is found, but arrow '${arrow.value}' isn't found in scope",
                  arrow.value,
                  abCtx.funcs.keys.toList.map(_.name)
                )
              )
              .as(none)
          ) { fn =>
            // TODO: add name and arrow separately
            // TODO: find tokens somewhere
            addServiceArrowLocation(name, arrow).as(fn.arrow.`type`.some)
          }
      case None =>
        report.error(name, "Ability with this name is undefined").as(none)
    }

  override def renameService(name: NamedTypeToken[S]): SX[Option[String]] =
    serviceExists(name.value).flatMap {
      case true =>
        mapStackHeadM(
          name.value.pure
        )(h =>
          mangler
            .rename(name.value)
            .map(newName => h.setServiceRename(name.value, newName) -> newName)
        ).map(_.some)
      case false =>
        report.error(name, "Service with this name is not registered").as(none)
    }

  override def getServiceRename(name: NamedTypeToken[S]): State[X, Option[String]] =
    (
      serviceExists(name.value),
      getState.map(_.getServiceRename(name.value))
    ).flatMapN {
      case (true, Some(rename)) => rename.some.pure
      case (false, _) => report.error(name, "Service with this name is undefined").as(none)
      case (_, None) => report.error(name, "Service ID is undefined").as(none)
    }

  override def beginScope(token: Token[S]): SX[Unit] =
    stackInt.beginScope(AbilitiesState.Frame[S](token))

  override def endScope(): SX[Unit] = stackInt.endScope

  private def serviceExists(name: String): SX[Boolean] =
    getState.map(_.services(name))

  private def getAbility(name: String): SX[Option[RawContext]] =
    getState.map(_.abilities.get(name))
}
