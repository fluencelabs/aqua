package aqua.model.transform

import aqua.model.*
import aqua.raw.ops.Call
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.{model, res}
import aqua.res.{CallRes, CallServiceRes, MakeRes}
import aqua.types.{ArrayType, LiteralType, ScalarType}
import aqua.types.StreamType
import aqua.model.IntoIndexModel
import aqua.model.inline.raw.ApplyGateRawInliner

import scala.language.implicitConversions
import cats.data.Chain
import cats.data.Chain.==:
import aqua.model.OnModel
import aqua.model.FailModel
import aqua.res.ResolvedOp

object ModelBuilder {
  implicit def rawToValue(raw: ValueRaw): ValueModel = ValueModel.fromRaw(raw)

  val relay = LiteralRaw("-relay-", ScalarType.string)

  val relayV = VarRaw("-relay-", ScalarType.string)

  val initPeer = ValueRaw.InitPeerId

  val emptyCall = Call(Nil, Nil)

  val otherPeer = VarRaw("other-peer", ScalarType.string)

  def otherPeerN(n: Int) = LiteralRaw.quote(s"other-peer-$n")
  def otherRelayN(n: Int) = LiteralRaw.quote(s"other-relay-$n")

  val otherPeerL = LiteralRaw.quote("other-peer")
  val otherRelay = LiteralRaw.quote("other-relay")
  val otherPeer2 = otherPeerN(2)
  val otherRelay2 = otherRelayN(2)
  val iRelay = VarRaw("i", ScalarType.string)
  val varNode = VarRaw("node-id", ScalarType.string)
  val viaList = VarRaw("other-relay-2", ArrayType(ScalarType.string))
  val valueArray = VarRaw("array", ArrayType(ScalarType.string))

  def callRes(
    i: Int,
    on: ValueModel,
    exportTo: Option[model.CallModel.Export] = None,
    args: List[ValueModel] = Nil
  ) =
    CallServiceRes(VarModel(s"srv$i", ScalarType.string), s"fn$i", CallRes(args, exportTo), on).leaf

  def callModel(i: Int, exportTo: List[CallModel.Export] = Nil, args: List[ValueRaw] = Nil) =
    CallServiceModel(
      VarRaw(s"srv$i", ScalarType.string),
      s"fn$i",
      CallModel(args.map(ValueModel.fromRaw), exportTo)
    ).leaf

  def callLiteralRes(i: Int, on: ValueModel, exportTo: Option[CallModel.Export] = None) =
    res
      .CallServiceRes(
        LiteralModel("\"srv" + i + "\"", LiteralType.string),
        s"fn$i",
        CallRes(Nil, exportTo),
        on
      )
      .leaf

  def callLiteralRaw(i: Int, exportTo: List[CallModel.Export] = Nil) =
    CallServiceModel(
      LiteralRaw.quote("srv" + i),
      s"fn$i",
      CallModel(Nil, exportTo)
    ).leaf

  def errorCall(bc: TransformConfig, i: Int, on: ValueModel = initPeer) =
    res
      .CallServiceRes(
        ValueModel.fromRaw(bc.errorHandlingSrvId),
        bc.errorFuncName,
        CallRes(
          ValueModel.lastError :: LiteralModel.number(i) :: Nil,
          None
        ),
        on
      )
      .leaf

  def respCall(bc: TransformConfig, value: ValueModel, on: ValueModel = initPeer) =
    res
      .CallServiceRes(
        ValueModel.fromRaw(bc.callbackSrvId),
        bc.respFuncName,
        CallRes(value :: Nil, None),
        on
      )
      .leaf

  def dataCall(bc: TransformConfig, name: String, on: ValueModel = initPeer) =
    res
      .CallServiceRes(
        ValueModel.fromRaw(bc.dataSrvId),
        name,
        CallRes(Nil, Some(CallModel.Export(name, ScalarType.string))),
        on
      )
      .leaf

  val failLastErrorModel = FailModel(ValueModel.lastError).leaf

  val failLastErrorRes = res.FailRes(ValueModel.lastError).leaf

  def onRethrowModel(
    peer: ValueModel,
    via: ValueModel*
  ): OpModel.Tree => OpModel.Tree =
    child =>
      XorModel.wrap(
        OnModel(peer, Chain.fromSeq(via)).wrap(
          child
        ),
        failLastErrorModel
      )

  def fold(item: String, iter: ValueRaw, mode: Option[ForModel.Mode], body: OpModel.Tree*) = {
    val ops = SeqModel.wrap(body: _*)
    ForModel(item, ValueModel.fromRaw(iter), mode).wrap(ops, NextModel(item).leaf)
  }

  def foldPar(item: String, iter: ValueRaw, body: OpModel.Tree*) = {
    val ops = SeqModel.wrap(body: _*)
    DetachModel.wrap(
      ForModel(item, ValueModel.fromRaw(iter), Some(ForModel.NeverMode))
        .wrap(ParModel.wrap(ops, NextModel(item).leaf))
    )
  }

  def through(peer: ValueModel): ResolvedOp.Tree =
    MakeRes.hop(peer)

  /**
   * @param stream stream [[VarModel]]
   * @param idx id [[ValueModel]]
   * @return [[OpModel.Tree]] of join of `stream[idx]`
   */
  def join(stream: VarModel, idx: ValueModel): OpModel.Tree =
    stream match {
      case VarModel(
            streamName,
            streamType: StreamType,
            Chain.`nil`
          ) =>
        ApplyGateRawInliner.joinStreamOnIndexModel(
          streamName = streamName,
          streamType = streamType,
          idxModel = idx,
          idxIncrName = streamName + "_incr",
          testName = streamName + "_test",
          iterName = streamName + "_fold_var",
          canonName = streamName + "_result_canon",
          iterCanonName = streamName + "_iter_canon",
          resultName = streamName + "_gate"
        )
      case _ => ???
    }
}
