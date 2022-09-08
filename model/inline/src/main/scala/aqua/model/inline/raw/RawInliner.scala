package aqua.model.inline.raw

import aqua.model.ValueModel
import aqua.model.inline.Inline
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.ValueRaw
import cats.data.State

trait RawInliner[T <: ValueRaw] {

  def apply[S: Mangler: Exports: Arrows](
    raw: T,
    lambdaAllowed: Boolean = true,
    canonicalizeStream: Boolean = false
  ): State[S, (ValueModel, Inline)]

}
