package aqua.model.transform

import aqua.model.{
  CallModel,
  CallServiceModel,
  ForModel,
  LiteralModel,
  NextModel,
  OpModel,
  ParModel,
  SeqModel,
  ValueModel,
  VarModel
}
import aqua.model.transform.funcop.ErrorsCatcher
import aqua.raw.ops.Call
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.{model, res}
import aqua.res.{CallRes, CallServiceRes, MakeRes}
import aqua.types.{ArrayType, LiteralType, ScalarType}

import scala.language.implicitConversions

object ModelBuilder {
  implicit def rawToValue(raw: ValueRaw): ValueModel = ValueModel.fromRaw(raw)

  val relay = LiteralRaw("-relay-", ScalarType.string)

  val relayV = VarRaw("-relay-", ScalarType.string)

  val initPeer = ValueRaw.InitPeerId

  val emptyCall = Call(Nil, Nil)

  val otherPeer = VarRaw("other-peer", ScalarType.string)

  val otherPeerL = LiteralRaw("\"other-peer\"", LiteralType.string)
  val otherRelay = LiteralRaw("other-relay", ScalarType.string)
  val otherPeer2 = LiteralRaw("other-peer-2", ScalarType.string)
  val otherRelay2 = LiteralRaw("other-relay-2", ScalarType.string)
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
        bc.errorHandlingCallback,
        bc.errorFuncName,
        CallRes(
          ErrorsCatcher.lastErrorArg :: LiteralModel(
            i.toString,
            LiteralType.number
          ) :: Nil,
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

  def fold(item: String, iter: ValueRaw, body: OpModel.Tree*) = {
    val ops = SeqModel.wrap(body: _*)
    ForModel(item, ValueModel.fromRaw(iter)).wrap(ops, NextModel(item).leaf)
  }

  def foldPar(item: String, iter: ValueRaw, body: OpModel.Tree*) = {
    val ops = SeqModel.wrap(body: _*)
    ForModel(item, ValueModel.fromRaw(iter)).wrap(ParModel.wrap(ops, NextModel(item).leaf))
  }

  def through(peer: ValueModel) =
    MakeRes.noop(peer)
}
