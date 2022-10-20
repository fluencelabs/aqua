package aqua.model.inline.raw

import aqua.model.{
  CallModel,
  CanonicalizeModel,
  NullModel,
  PushToStreamModel,
  RestrictionModel,
  SeqModel,
  ValueModel,
  VarModel,
  XorModel
}
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.valueToModel
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.CollectionRaw
import aqua.types.{ArrayType, CanonStreamType, OptionType, StreamType}
import cats.data.{Chain, State}

object CollectionRawInliner extends RawInliner[CollectionRaw] {

  override def apply[S: Mangler: Exports: Arrows](
    raw: CollectionRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] =
    for {
      streamName <-
        raw.boxType match {
          case _: StreamType => raw.assignToName.map(s => State.pure(s)).getOrElse(Mangler[S].findAndForbidName("stream-inline"))
          case _: CanonStreamType => Mangler[S].findAndForbidName("canon_stream-inline")
          case _: ArrayType => Mangler[S].findAndForbidName("array-inline")
          case _: OptionType => Mangler[S].findAndForbidName("option-inline")
        }

      stream = VarModel(streamName, StreamType(raw.elementType))
      streamExp = CallModel.Export(stream.name, stream.`type`)

      vals <- raw.values
        .traverse(valueToModel(_))
        .map(_.toList)
        .map(Chain.fromSeq)
        .map(_.flatMap { case (v, t) =>
          Chain.fromOption(t) :+ PushToStreamModel(v, streamExp).leaf
        })

      canonName <-
        if (raw.boxType.isStream) State.pure(streamName)
        else Mangler[S].findAndForbidName(streamName)
      canonType = raw.boxType match {
        case StreamType(_) => raw.boxType
        case _ => CanonStreamType(raw.boxType.element)
      }
      canon = CallModel.Export(canonName, canonType)
    } yield VarModel(canonName, canon.`type`) -> Inline.tree(
      raw.boxType match {
        case ArrayType(_) =>
          RestrictionModel(streamName, isStream = true).wrap(
            SeqModel.wrap((vals :+ CanonicalizeModel(stream, canon).leaf).toList: _*)
          )
        case OptionType(_) =>
          RestrictionModel(streamName, isStream = true).wrap(
            SeqModel.wrap(
              XorModel.wrap((vals :+ NullModel.leaf).toList: _*),
              CanonicalizeModel(stream, canon).leaf
            )
          )
        case _ =>
          SeqModel.wrap(vals.toList: _*)
      }
    )
}
