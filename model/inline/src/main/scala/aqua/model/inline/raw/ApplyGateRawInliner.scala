package aqua.model.inline.raw

import aqua.model.*
import aqua.model.inline.Inline
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.{ApplyBinaryOpRaw, ApplyGateRaw, LiteralRaw, ValueRaw, VarRaw}
import aqua.model.inline.RawValueInliner.unfold
import aqua.types.{ArrayType, CanonStreamType, ScalarType, StreamType}

import cats.data.State
import cats.data.Chain
import cats.syntax.monoid.*
import cats.syntax.option.*
import scribe.Logging

object ApplyGateRawInliner extends RawInliner[ApplyGateRaw] with Logging {

  /**
   * To wait for the element of a stream by the given index, the following model is generated:
   * (seq
   * (seq
   *  (fold $stream s
   *   (seq
   *    (seq
   *     (ap s $stream_test)
   *     (canon <peer> $stream_test  #stream_iter_canon)
   *    )
   *    (xor
   *     (match #stream_iter_canon.length idx
   *      (null)
   *     )
   *     (next s)
   *    )
   *   )
   *   (never)
   *  )
   *  (canon <peer> $stream_test  #stream_result_canon)
   * )
   * (ap #stream_result_canon stream_gate)
   * )
   */
  def joinStreamOnIndexModel(
    streamName: String,
    streamType: StreamType,
    idxModel: ValueModel,
    testName: String,
    iterName: String,
    canonName: String,
    iterCanonName: String,
    resultName: String
  ): OpModel.Tree = {
    val varSTest = VarModel(testName, streamType)
    val iter = VarModel(iterName, streamType.element)

    val iterCanon = VarModel(iterCanonName, CanonStreamType(streamType.element))

    val resultCanon =
      VarModel(canonName, CanonStreamType(streamType.element))

    RestrictionModel(varSTest.name, streamType).wrap(
      ForModel(iter.name, VarModel(streamName, streamType), ForModel.Mode.Never.some).wrap(
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
              .copy(properties = Chain.one(FunctorModel("length", ScalarType.`u32`))),
            idxModel,
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

  override def apply[S: Mangler: Exports: Arrows](
    afr: ApplyGateRaw,
    propertyAllowed: Boolean
  ): State[S, (ValueModel, Inline)] =
    for {
      uniqueCanonName <- Mangler[S].findAndForbidName(afr.name + "_result_canon")
      uniqueResultName <- Mangler[S].findAndForbidName(afr.name + "_gate")
      uniqueTestName <- Mangler[S].findAndForbidName(afr.name + "_test")
      uniqueIdxIncr <- Mangler[S].findAndForbidName(afr.name + "_incr")
      uniqueIterCanon <- Mangler[S].findAndForbidName(afr.name + "_iter_canon")
      uniqueIter <- Mangler[S].findAndForbidName(afr.name + "_fold_var")
      /**
       * NOTE: Incrementing at `ValueRaw` level here
       * to apply possible optimizations to the increment value
       */
      idx <- unfold(increment(afr.idx))
      (idxModel, idxInline) = idx
    } yield {
      val gate = joinStreamOnIndexModel(
        streamName = afr.name,
        streamType = afr.streamType,
        idxModel = idxModel,
        testName = uniqueTestName,
        iterName = uniqueIter,
        canonName = uniqueCanonName,
        iterCanonName = uniqueIterCanon,
        resultName = uniqueResultName
      )

      val tree = SeqModel.wrap(idxInline.predo.toList :+ gate)

      val treeInline = Inline(predo = Chain.one(tree))

      (
        VarModel(uniqueResultName, ArrayType(afr.streamType.element)),
        treeInline
      )

    }

  private def increment(v: ValueRaw): ValueRaw =
    ApplyBinaryOpRaw.Add(v, LiteralRaw.number(1))
}
