package aqua.model.inline.state

import aqua.mangler.ManglerState
import aqua.model.{FuncArrow, ValueModel}
import aqua.model.inline.state.{Arrows, Counter, Exports, Mangler}
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
  resolvedExports: Map[String, ValueModel] = Map.empty,
  resolvedArrows: Map[String, FuncArrow] = Map.empty,
  instructionCounter: Int = 0
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

}
