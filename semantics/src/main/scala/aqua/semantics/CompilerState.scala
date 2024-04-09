package aqua.semantics

import aqua.mangler.ManglerState
import aqua.parser.lexer.Token
import aqua.raw.{Raw, RawContext}
import aqua.semantics.rules.abilities.AbilitiesState
import aqua.semantics.rules.definitions.DefinitionsState
import aqua.semantics.rules.locations.LocationsState
import aqua.semantics.rules.names.NamesState
import aqua.semantics.rules.report.ReportState
import aqua.semantics.rules.types.TypesState

import cats.Semigroup
import cats.data.{Chain, State}
import cats.kernel.Monoid
import cats.syntax.monoid.*
import monocle.Lens
import monocle.macros.GenLens

case class CompilerState[S[_]](
  report: ReportState[S] = ReportState[S](),
  mangler: ManglerState = ManglerState(),
  names: NamesState[S] = NamesState[S](),
  abilities: AbilitiesState[S] = AbilitiesState[S](),
  types: TypesState[S] = TypesState[S](),
  definitions: DefinitionsState[S] = DefinitionsState[S](),
  locations: LocationsState[S] = LocationsState[S]()
) {

  lazy val errors: Chain[SemanticError[S]] = report.errors
  lazy val warnings: Chain[SemanticWarning[S]] = report.warnings
}

object CompilerState {
  type St[S[_]] = State[CompilerState[S], Raw]

  def init[F[_]](ctx: RawContext): CompilerState[F] =
    CompilerState(
      names = NamesState.init[F](ctx),
      abilities = AbilitiesState.init[F](ctx),
      types = TypesState.init[F](ctx)
    )

  given [S[_]]: Lens[CompilerState[S], ReportState[S]] =
    GenLens[CompilerState[S]](_.report)

  given [S[_]]: Lens[CompilerState[S], NamesState[S]] =
    GenLens[CompilerState[S]](_.names)

  given [S[_]]: Lens[CompilerState[S], AbilitiesState[S]] =
    GenLens[CompilerState[S]](_.abilities)

  given [S[_]]: Lens[CompilerState[S], ManglerState] =
    GenLens[CompilerState[S]](_.mangler)

  given [S[_]]: Lens[CompilerState[S], TypesState[S]] =
    GenLens[CompilerState[S]](_.types)

  given [S[_]]: Lens[CompilerState[S], DefinitionsState[S]] =
    GenLens[CompilerState[S]](_.definitions)

  given [S[_]]: Monoid[St[S]] with {
    override def empty: St[S] = State.pure(Raw.Empty("compiler state monoid empty"))

    override def combine(x: St[S], y: St[S]): St[S] = for {
      a <- x.get
      b <- y.get
      _ <- State.set(
        CompilerState[S](
          a.report |+| b.report,
          a.mangler |+| b.mangler,
          a.names |+| b.names,
          a.abilities |+| b.abilities,
          a.types |+| b.types,
          locations = a.locations |+| b.locations
        )
      )
      am <- x
      ym <- y
    } yield {
      // println(s"MONOID COMBINE $am $ym")
      am |+| ym
    }
  }

}
