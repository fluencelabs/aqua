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

import aqua.model.inline.Inline
import aqua.model.inline.Inline.MergeMode.*
import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.state.*
import aqua.model.{
  CallModel,
  CanonicalizeModel,
  FlattenModel,
  FunctorModel,
  LiteralModel,
  SeqModel,
  ValueModel,
  VarModel
}
import aqua.raw.value.{FunctorRaw, ValueRaw}
import aqua.types.{ArrayType, CanonStreamType, CollectionType, StreamType}

import cats.data.Chain
import cats.data.State
import cats.syntax.monoid.*
import scribe.Logging

object ApplyFunctorRawInliner extends Logging {

  def apply[S: Mangler: Exports: Arrows: Config](
    value: ValueModel,
    functor: FunctorRaw
  ): State[S, (VarModel, Inline)] = {
    val functorModel = FunctorModel(functor.name, functor.`type`)

    value match {
      case v @ VarModel(name, bt, _) =>
        for {
          apName <- Mangler[S].findAndForbidName(name + "_to_functor")
          resultName <- Mangler[S].findAndForbidName(s"${name}_${functor.name}")
          (apVar, flat) = {
            bt match {
              case StreamType(el) =>
                val canonType = CanonStreamType(el)
                (
                  VarModel(apName, canonType, Chain.one(functorModel)),
                  CanonicalizeModel(v, CallModel.Export(apName, canonType)).leaf
                )
              case CanonStreamType(el) =>
                val arrType = ArrayType(el)
                (
                  VarModel(apName, arrType, Chain.one(functorModel)),
                  FlattenModel(v, apName).leaf
                )
              case _ =>
                (VarModel(apName, bt, Chain.one(functorModel)), FlattenModel(v, apName).leaf)
            }
          }
        } yield {
          val tree = Inline(
            predo = Chain.one(
              SeqModel.wrap(
                flat,
                FlattenModel(apVar, resultName).leaf
              )
            ),
            mergeMode = SeqMode
          )

          VarModel(resultName, functor.`type`) -> tree
        }
      case l @ LiteralModel(_, _) =>
        ApplyPropertiesRawInliner.flatLiteralWithProperties(
          l,
          Inline.empty,
          Chain.one(functorModel)
        )
    }
  }
}
