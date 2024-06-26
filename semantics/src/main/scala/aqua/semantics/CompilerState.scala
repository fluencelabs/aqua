/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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

}
