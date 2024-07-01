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

package aqua.model.inline.state

import aqua.mangler.ManglerState
import aqua.model.inline.state.Exports.ExportsState
import aqua.model.inline.state.{Arrows, Counter, Exports, Mangler}
import aqua.model.{FuncArrow, ValueModel}
import aqua.raw.arrow.FuncRaw
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.ArrowType

import cats.data.{Chain, State}
import cats.instances.list.*
import cats.syntax.traverse.*
import scribe.Logging

/**
 * Default states aggregate for all available state algebras
 *
 * @param noNames
 *   for [[Mangler]]
 * @param resolvedExports
 *   for [[Exports]]
 * @param resolvedArrows
 *   for [[Arrows]]
 * @param instructionCounter
 *   for [[Counter]]
 */
case class InliningState(
  noNames: ManglerState = ManglerState(),
  resolvedExports: ExportsState = ExportsState(),
  resolvedArrows: Map[String, FuncArrow] = Map.empty,
  instructionCounter: Int = 0,
  config: Config.Values = Config.Values.default
)

object InliningState {

  given Counter[InliningState] =
    Counter.Simple.transformS(_.instructionCounter, (acc, i) => acc.copy(instructionCounter = i))

  given Mangler[InliningState] =
    Mangler[ManglerState].transformS(_.noNames, (acc, nn) => acc.copy(noNames = nn))

  given Arrows[InliningState] =
    Arrows.Simple.transformS(_.resolvedArrows, (acc, aa) => acc.copy(resolvedArrows = aa))

  given Exports[InliningState] =
    Exports.Simple.transformS(_.resolvedExports, (acc, ex) => acc.copy(resolvedExports = ex))

  given Config[InliningState] =
    Config[Config.Values].transform(_.config)
}
