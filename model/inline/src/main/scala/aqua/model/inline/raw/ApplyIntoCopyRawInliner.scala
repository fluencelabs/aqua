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

import aqua.errors.Errors.internalError
import aqua.model.*
import aqua.model.inline.Inline.MergeMode.*
import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.state.*
import aqua.model.inline.{Inline, MakeStructRawInliner}
import aqua.raw.value.IntoCopyRaw
import aqua.types.{StreamMapType, StructType}

import cats.data.{Chain, NonEmptyMap, State}
import cats.syntax.foldable.*
import cats.syntax.functor.*
import scribe.Logging

object ApplyIntoCopyRawInliner extends Logging {

  private def copyObj[S: Mangler](
    oldValue: VarModel,
    resultName: String,
    resultType: StructType,
    fields: NonEmptyMap[String, ValueModel]
  ): State[S, OpModel.Tree] = {
    val mapType = StreamMapType.top()
    val mapName = resultName + "_map"

    val nonCopiedValues = resultType.fields.toNel.filterNot { case (k, _) =>
      fields.contains(k)
    }.flatMap { case (k, _) =>
      oldValue
        .intoField(k)
        .map(vm =>
          InsertKeyValueModel(
            LiteralModel.quote(k),
            vm,
            mapName,
            mapType
          ).leaf
        )
    }

    MakeStructRawInliner
      .constructThroughMap(
        mapName,
        mapType,
        CallModel.Export(resultName, resultType),
        fields,
        nonCopiedValues
      )
  }

  def apply[S: Mangler: Exports: Arrows: Config](
    value: VarModel,
    intoCopy: IntoCopyRaw
  ): State[S, (VarModel, Inline)] = {
    value.`type` match {
      case st: StructType =>
        for {
          resultName <- Mangler[S].findAndForbidName(st.name + "_obj_copy")
          foldedFields <- intoCopy.fields.nonEmptyTraverse(unfold(_))
          varModel = VarModel(resultName, st)
          valsInline = foldedFields.toList.foldMap { case (_, inline) => inline }.desugar
          fields = foldedFields.map(_._1)
          objCopy <- copyObj(value, resultName, st, fields)
        } yield {
          (
            varModel,
            Inline(
              Chain.one(SeqModel.wrap(valsInline.predo :+ objCopy)),
              SeqMode
            )
          )
        }
      case _ =>
        internalError("Unreachable. Cannot copy a value that is not a data type")
    }

  }
}
