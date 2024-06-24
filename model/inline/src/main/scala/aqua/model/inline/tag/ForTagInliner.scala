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

package aqua.model.inline.tag

import aqua.errors.Errors.internalError
import aqua.helpers.syntax.reader.*
import aqua.model.*
import aqua.model.ValueModel
import aqua.model.inline.Inline.parDesugarPrefixOpt
import aqua.model.inline.RawValueInliner.valueToModel
import aqua.model.inline.TagInliner.TagInlined
import aqua.model.inline.TagInliner.flat
import aqua.model.inline.state.*
import aqua.raw.ops.{ForKeyValue, ForTag}
import aqua.raw.value.ValueRaw
import aqua.types.{CollectionType, ScalarType, StreamType}

import cats.Eval
import cats.data.Reader
import cats.data.{Chain, State}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.traverse.*

final case class ForTagInliner(
                                item: String,
                                iterable: ValueRaw,
                                mode: ForTag.Mode,
                                keyValue: Option[ForKeyValue]
                              ) {

  def inlined[S: Mangler: Exports: Arrows: Config]: State[S, TagInlined[S]] = for {
    vp <- valueToModel(iterable)
    flattened <- mode match {
      case ForTag.Mode.RecMode => State.pure(vp)
      case _ => flat.tupled(vp)
    }
    (v, p) = flattened
    n <- Mangler[S].findAndForbidName(item)
    elementType = iterable.`type` match {
      case b: CollectionType => b.element
      case _ =>
        internalError(
          s"non-box type variable '$iterable' in 'for' expression."
        )
    }
    itemVar = VarModel(n, elementType)
    _ <- Exports[S].resolved(item, itemVar)
    pref <- keyValue.traverse(kv =>
      for {
        keyName <- Mangler[S].findAndForbidName(kv.key)
        _ <- Exports[S].resolved(kv.key, VarModel(keyName, ScalarType.string))
        valueName <- Mangler[S].findAndForbidName(kv.value)
        _ <- Exports[S].resolved(kv.value, VarModel(valueName, elementType))
      } yield SeqModel.wrap(
        FlattenModel(
          itemVar.withProperty(IntoFieldModel("key", ScalarType.string)),
          keyName
        ).leaf,
        FlattenModel(
          itemVar.withProperty(IntoFieldModel("value", elementType)),
          valueName
        ).leaf
      )
    )
    modeModel = mode match {
      case ForTag.Mode.SeqMode | ForTag.Mode.TryMode => ForModel.Mode.Null
      case ForTag.Mode.ParMode | ForTag.Mode.RecMode => ForModel.Mode.Never
    }
    model = ForModel(n, v, modeModel)
  } yield TagInlined.Around(
    model = StreamRestrictions.restrictStreams(ss => model.wrap(Chain.fromOption(pref) ++ ss)),
    prefix = p
  )
}
