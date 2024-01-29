package aqua.model.inline.state

import aqua.mangler.ManglerState
import aqua.model.inline.state.Exports.ExportsState
import aqua.model.inline.state.{Counter, Exports, Mangler}
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
  instructionCounter: Int = 0
)

object InliningState {

  given Counter[InliningState] =
    Counter.Simple.transformS(_.instructionCounter, (acc, i) => acc.copy(instructionCounter = i))

  given Mangler[InliningState] =
    Mangler[ManglerState].transformS(_.noNames, (acc, nn) => acc.copy(noNames = nn))

  given Exports[InliningState] =
    Exports[ExportsState].transformS(_.resolvedExports, (acc, ex) => acc.copy(resolvedExports = ex))

}
