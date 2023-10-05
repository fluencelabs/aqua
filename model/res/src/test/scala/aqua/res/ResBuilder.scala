package aqua.res

import aqua.model.*
import aqua.types.*
import aqua.raw.value.*

import cats.data.Chain
import cats.syntax.option.*

object ResBuilder {

  /**
   * Model of waiting `onIdx` elements of `stream`
   */
  def join(stream: VarModel, onIdx: ValueModel, peer: ValueModel) = {
    val testVM = VarModel(stream.name + "_test", stream.`type`)
    val testStreamType = stream.`type`.asInstanceOf[StreamType] // Unsafe
    val iter = VarModel(stream.name + "_fold_var", ScalarType.string)
    val canon = VarModel(stream.name + "_iter_canon", CanonStreamType(ScalarType.string))
    val canonRes = VarModel(stream.name + "_result_canon", CanonStreamType(ScalarType.string))
    val arrayRes = VarModel(stream.name + "_gate", ArrayType(ScalarType.string))

    RestrictionRes(testVM.name, testStreamType).wrap(
      FoldRes(iter.name, stream, ForModel.Mode.Never.some).wrap(
        ApRes(iter, CallModel.Export(testVM.name, testVM.`type`)).leaf,
        CanonRes(testVM, peer, CallModel.Export(canon.name, canon.`type`)).leaf,
        XorRes.wrap(
          MatchMismatchRes(
            canon.copy(properties = Chain.one(FunctorModel("length", ScalarType.u32))),
            onIdx,
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
