package aqua.model.inline.raw

import aqua.model.*
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.valueToModel
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.CollectionRaw
import aqua.types.StreamMapType
import aqua.types.{ArrayType, CanonStreamType, OptionType, StreamType}

import cats.data.{Chain, State}

object CollectionRawInliner extends RawInliner[CollectionRaw] {

  override def apply[S: Mangler: Exports: Arrows](
    raw: CollectionRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] =
    for {
      streamName <- raw.collectionType match {
        case _: ArrayType => Mangler[S].findAndForbidName("array-inline")
        case _: OptionType => Mangler[S].findAndForbidName("option-inline")
        // CanonStreamType is here just to avoid compilation warning. Right now it is unreachable
        case _: CanonStreamType => Mangler[S].findAndForbidName("canon_stream-inline")
      }

      streamType = StreamType(raw.elementType)
      stream = VarModel(streamName, streamType)
      streamExp = CallModel.Export(stream.name, stream.`type`)

      valsWithInlines <- raw.values
        .traverse(valueToModel(_))
        .map(_.toList)
        .map(Chain.fromSeq)

      // push values to the stream, that is gathering the collection
      vals = valsWithInlines.map { case (v, _) =>
        PushToStreamModel(v, streamExp).leaf
      }

      // all inlines will be added before pushing values to the stream
      inlines = valsWithInlines.flatMap { case (_, t) =>
        Chain.fromOption(t)
      }

      canonName <-
        if (raw.collectionType.isStream) State.pure(streamName)
        else Mangler[S].findAndForbidName(streamName)
      canonType = CanonStreamType(raw.collectionType.element)
      canon = CallModel.Export(canonName, canonType)
    } yield VarModel(canonName, canon.`type`) -> Inline.tree(
      raw.collectionType match {
        case ArrayType(_) =>
          RestrictionModel(streamName, streamType).wrap(
            SeqModel.wrap(inlines ++ vals :+ CanonicalizeModel(stream, canon).leaf)
          )
        case OptionType(_) =>
          RestrictionModel(streamName, streamType).wrap(
            SeqModel.wrap(
              SeqModel.wrap(inlines),
              XorModel.wrap(vals :+ NullModel.leaf),
              CanonicalizeModel(stream, canon).leaf
            )
          )
        case _ =>
          SeqModel.wrap(inlines ++ vals)
      }
    )
}
