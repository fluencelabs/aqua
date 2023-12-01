package aqua.model.inline.raw

import aqua.model.*
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.valueToModel
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.StreamRaw

import cats.data.{Chain, State}
import cats.syntax.traverse.*

object StreamRawInliner extends RawInliner[StreamRaw] {

  override def apply[S: Mangler: Exports: Arrows](
    raw: StreamRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = {
    val streamExp = CallModel.Export(raw.streamName, raw.streamType)
    val streamVal = streamExp.asVar
    for {
      valsWithInlines <- raw.values
        .traverse(valueToModel(_))
        .map(Chain.fromSeq)

      // push values to the stream, that is gathering the collection
      vals = valsWithInlines.map { case (v, _) =>
        PushToStreamModel(v, streamExp).leaf
      }

      // all inlines will be added before pushing values to the stream
      inlines = valsWithInlines.flatMap { case (_, t) =>
        Chain.fromOption(t)
      }

      _ <- Exports[S].resolved(raw.streamName, streamVal)
    } yield streamVal -> Inline.tree(
      SeqModel.wrap(inlines ++ vals)
    )
  }
}
