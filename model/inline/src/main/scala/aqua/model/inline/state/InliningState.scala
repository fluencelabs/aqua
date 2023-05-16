package aqua.model.inline.state

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
  noNames: Set[String] = Set.empty,
  resolvedExports: Map[String, ValueModel] = Map.empty,
  resolvedArrows: Map[String, FuncArrow] = Map.empty,
  instructionCounter: Int = 0,
  scopes: Map[String, ScopeLinks] = Map.empty
)

object InliningState {

  given Counter[InliningState] =
    Counter.Simple.transformS(_.instructionCounter, (acc, i) => acc.copy(instructionCounter = i))

  given Mangler[InliningState] =
    Mangler.Simple.transformS(_.noNames, (acc, nn) => acc.copy(noNames = nn))

  given Arrows[InliningState] =
    Arrows.Simple.transformS(_.resolvedArrows, (acc, aa) => acc.copy(resolvedArrows = aa))

  given Exports[InliningState] =
    Exports.Simple.transformS(_.resolvedExports, (acc, ex) => acc.copy(resolvedExports = ex))

  given Scopes[InliningState] =
    Scopes.Simple.transformS(s => ScopeState(s.scopes, s.resolvedExports, s.resolvedArrows), (acc, ex) => acc.copy(scopes = ex.links))

}
