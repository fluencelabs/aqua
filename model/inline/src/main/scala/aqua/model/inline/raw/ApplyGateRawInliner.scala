package aqua.model.inline.raw

import aqua.model.*
import aqua.model.inline.Inline
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.{ApplyGateRaw, LiteralRaw, VarRaw}
import cats.data.State
import cats.data.Chain
import aqua.model.inline.RawValueInliner.unfold
import aqua.types.{CanonStreamType, ScalarType, StreamType, ArrayType}
import cats.syntax.monoid.*
import scribe.Logging

object ApplyGateRawInliner extends RawInliner[ApplyGateRaw] with Logging {

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
      idxFolded <- unfold(afr.idx)
      (idxModel, idxInline) = idxFolded
    } yield {
      val varSTest = VarModel(uniqueTestName, afr.streamType)
      val iter = VarModel(uniqueIter, afr.streamType.element)

      val iterCanon = VarModel(uniqueIterCanon, CanonStreamType(afr.streamType.element))

      val resultCanon =
        VarModel(uniqueCanonName, CanonStreamType(afr.streamType.element))

      val incrVar = VarModel(uniqueIdxIncr, ScalarType.u32)

      // To wait for the element of a stream by the given index, the following model is generated:
      // (seq
      // (seq
      //  (seq
      //   (call %init_peer_id% ("math" "add") [0 1] stream_incr)
      //   (fold $stream s
      //    (seq
      //     (seq
      //      (ap s $stream_test)
      //      (canon %init_peer_id% $stream_test  #stream_iter_canon)
      //     )
      //     (xor
      //      (match #stream_iter_canon.length stream_incr
      //       (null)
      //      )
      //      (next s)
      //     )
      //    )
      //    (never)
      //   )
      //  )
      //  (canon %init_peer_id% $stream_test  #stream_result_canon)
      // )
      // (ap #stream_result_canon stream_gate)
      // )
      val gate = RestrictionModel(varSTest.name, true).wrap(
        increment(idxModel, incrVar),
        ForModel(iter.name, VarModel(afr.name, afr.streamType), Some(ForModel.NeverMode)).wrap(
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
              incrVar,
              true
            ).leaf,
            NextModel(iter.name).leaf
          )
        ),
        CanonicalizeModel(
          varSTest,
          CallModel.Export(resultCanon.name, CanonStreamType(afr.streamType.element))
        ).leaf,
        FlattenModel(
          resultCanon,
          uniqueResultName
        ).leaf
      )

      val tree = SeqModel.wrap((idxInline.predo.toList :+ gate):_*)

      val treeInline =
        Inline(idxInline.flattenValues, predo = Chain.one(tree))

      (
        VarModel(uniqueResultName, ArrayType(afr.streamType.element)),
        treeInline
      )

    }

  private def increment(v: ValueModel, result: VarModel) =
    CallServiceModel(
      LiteralModel("\"math\"", ScalarType.string),
      "add",
      CallModel(
        v :: LiteralModel.fromRaw(LiteralRaw.number(1)) :: Nil,
        CallModel.Export(result.name, result.`type`) :: Nil
      )
    ).leaf
}
