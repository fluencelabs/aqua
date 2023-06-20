package aqua.res

import aqua.model.*
import aqua.types.*
import aqua.raw.value.*

import cats.data.Chain

object ResBuilder {

  def join(stream: VarModel, onIdx: ValueModel, peer: ValueModel) = {
    val testVM = VarModel(stream.name + "_test", stream.`type`)
    val testStreamType = stream.`type`.asInstanceOf[StreamType] // Unsafe
    val iter = VarModel(stream.name + "_fold_var", ScalarType.string)
    val canon = VarModel(stream.name + "_iter_canon", CanonStreamType(ScalarType.string))
    val canonRes = VarModel(stream.name + "_result_canon", CanonStreamType(ScalarType.string))
    val arrayRes = VarModel(stream.name + "_gate", ArrayType(ScalarType.string))
    val idx = VarModel(stream.name + "_incr", ScalarType.u32)

    RestrictionRes(testVM.name, testStreamType).wrap(
      CallServiceRes(
        LiteralModel("\"math\"", ScalarType.string),
        "add",
        CallRes(
          onIdx :: LiteralModel.fromRaw(LiteralRaw.number(1)) :: Nil,
          Some(CallModel.Export(idx.name, idx.`type`))
        ),
        peer
      ).leaf,
      FoldRes(iter.name, stream, Some(ForModel.NeverMode)).wrap(
        ApRes(iter, CallModel.Export(testVM.name, testVM.`type`)).leaf,
        CanonRes(testVM, peer, CallModel.Export(canon.name, canon.`type`)).leaf,
        XorRes.wrap(
          MatchMismatchRes(
            canon.copy(properties = Chain.one(FunctorModel("length", ScalarType.u32))),
            idx,
            true
          ).leaf,
          NextRes(iter.name).leaf
        )
      ),
      CanonRes(testVM, peer, CallModel.Export(canonRes.name, canonRes.`type`)).leaf,
      ApRes(canonRes, CallModel.Export(arrayRes.name, arrayRes.`type`)).leaf
    )
  }

}
