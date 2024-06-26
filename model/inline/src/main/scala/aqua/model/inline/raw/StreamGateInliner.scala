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
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.state.*
import aqua.types.{ArrayType, CanonStreamType, ScalarType, StreamType}

import cats.data.Chain
import cats.data.State
import cats.syntax.applicative.*
import cats.syntax.monoid.*
import cats.syntax.option.*
import scribe.Logging

object StreamGateInliner extends Logging {

  /**
   * To wait for size elements of a stream,
   * the following model is generated:
   * (seq
   *  (seq
   *   (fold $stream s
   *    (seq
   *     (ap s $stream_test)
   *     (canon <peer> $stream_test  #stream_iter_canon)
   *    )
   *    (xor
   *     (match #stream_iter_canon.length size
   *      (null)
   *     )
   *     (next s)
   *    )
   *    (never)
   *   )
   *   (canon <peer> $stream_test  #stream_result_canon)
   *  )
   *  (ap #stream_result_canon stream_gate)
   * )
   */
  def joinStreamOnIndexModel(
    streamName: String,
    streamType: StreamType,
    sizeModel: ValueModel,
    testName: String,
    iterName: String,
    canonName: String,
    iterCanonName: String,
    resultName: String
  ): OpModel.Tree = {
    val varSTest = VarModel(testName, streamType)
    val iter = VarModel(iterName, streamType.element)
    val iterCanon = VarModel(iterCanonName, CanonStreamType(streamType.element))
    val resultCanon = VarModel(canonName, CanonStreamType(streamType.element))

    RestrictionModel(varSTest.name, streamType).wrap(
      ForModel(iter.name, VarModel(streamName, streamType), ForModel.Mode.Never).wrap(
        PushToStreamModel(
          iter,
          CallModel.Export(varSTest.name, varSTest.`type`)
        ).leaf,
        CanonicalizeModel(
          varSTest,
          CallModel.Export(iterCanon.name, iterCanon.`type`)
        ).leaf,
        XorModel.wrap(
          MatchMismatchModel(
            iterCanon
              .withProperty(
                FunctorModel("length", ScalarType.`u32`)
              ),
            sizeModel,
            true
          ).leaf,
          NextModel(iter.name).leaf
        )
      ),
      CanonicalizeModel(
        varSTest,
        CallModel.Export(resultCanon.name, CanonStreamType(streamType.element))
      ).leaf,
      FlattenModel(
        resultCanon,
        resultName
      ).leaf
    )
  }

  def apply[S: Mangler: Exports: Arrows: Config](
    streamName: String,
    streamType: StreamType,
    sizeModel: ValueModel
  ): State[S, (VarModel, Inline)] =
    for {
      uniqueCanonName <- Mangler[S].findAndForbidName(streamName + "_result_canon")
      uniqueResultName <- Mangler[S].findAndForbidName(streamName + "_gate")
      uniqueTestName <- Mangler[S].findAndForbidName(streamName + "_test")
      uniqueIterCanon <- Mangler[S].findAndForbidName(streamName + "_iter_canon")
      uniqueIter <- Mangler[S].findAndForbidName(streamName + "_fold_var")
    } yield {
      val gate = joinStreamOnIndexModel(
        streamName = streamName,
        streamType = streamType,
        sizeModel = sizeModel,
        testName = uniqueTestName,
        iterName = uniqueIter,
        canonName = uniqueCanonName,
        iterCanonName = uniqueIterCanon,
        resultName = uniqueResultName
      )

      val inline = Inline(predo = Chain.one(gate))
      val value = VarModel(
        uniqueResultName,
        ArrayType(streamType.element)
      )

      (value, inline)
    }
}
