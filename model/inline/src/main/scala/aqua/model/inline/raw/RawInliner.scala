package aqua.model.inline.raw

import aqua.model.ValueModel
import aqua.model.inline.Inline
import aqua.model.inline.state.Exports.Export
import aqua.model.inline.state.{Exports, Mangler}
import aqua.raw.value.ValueRaw

import cats.data.State

trait RawInliner[T <: ValueRaw] {

  def apply[S: Mangler: Exports](
    raw: T
  ): State[S, (Export, Inline)]

}
