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

package aqua.model.inline.raw

import aqua.model.*
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.valueToModel
import aqua.model.inline.state.*
import aqua.raw.value.CollectionRaw
import aqua.types.StreamMapType
import aqua.types.{ArrayType, CanonStreamType, OptionType, StreamType}

import cats.data.{Chain, State}

object CollectionRawInliner extends RawInliner[CollectionRaw] {

  override def apply[S: Mangler: Exports: Arrows: Config](
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
      vals = valsWithInlines.map { case (v, tOp) =>
        val pts = PushToStreamModel(v, streamExp).leaf
        tOp.map(t => SeqModel.wrap(Chain(t, pts))).getOrElse(pts)
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
            SeqModel.wrap(vals :+ CanonicalizeModel(stream, canon).leaf)
          )
        case OptionType(_) =>
          RestrictionModel(streamName, streamType).wrap(
            SeqModel.wrap(
              XorModel.wrap(vals :+ NullModel.leaf),
              CanonicalizeModel(stream, canon).leaf
            )
          )
        case _ =>
          SeqModel.wrap(vals)
      }
    )
}
