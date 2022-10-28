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
      uniqueIdxIncr <- Mangler[S].findAndForbidName(afr.idxName + "_incr")
    } yield {
      val varSTest = VarModel(uniqueTestName, afr.streamType)
      val iter = VarModel("s", afr.streamType.element)

      val iterCanon = VarModel(afr.name + "_iter_canon", CanonStreamType(afr.streamType.element))

      val resultCanon =
        VarModel(uniqueCanonName, CanonStreamType(afr.streamType.element))

      val incrVar = VarModel(uniqueIdxIncr, ScalarType.u32)

      val tree = RestrictionModel(varSTest.name, true).wrap(
        increment(VarModel(afr.idxName, afr.idxType), incrVar),
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

      val treeInline =
        Inline.tree(tree)

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
