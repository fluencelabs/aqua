package aqua.model.transform.pre

import aqua.model.OpModel.Tree
import aqua.model.{
  CallModel,
  CallServiceModel,
  LiteralModel,
  OnModel,
  OpModel,
  ValueModel,
  VarModel
}
import aqua.raw.ops.{Call, CallArrowRawTag, OnTag, RawTag}
import aqua.raw.value.{ValueRaw, VarRaw}
import cats.data.Chain
import aqua.types.Type

sealed trait InitPeerCallable extends PreTransform {

  def onInitPeer: OnModel

  def makeCall(serviceId: ValueRaw, funcName: String, call: Call): RawTag.Tree =
    transform(CallArrowRawTag.service(serviceId, funcName, call).leaf)

  def service(serviceId: ValueRaw): (String, Call) => RawTag.Tree =
    makeCall(serviceId, _, _)
}

// TODO: refactor goThrough into some supertype of VarRaw and VarModel with no lambda
case class InitViaRelayCallable(goThrough: Chain[(String, Type)]) extends InitPeerCallable {

  // Get to init user through a relay
  override def transform(op: RawTag.Tree): RawTag.Tree =
    OnTag(
      ValueRaw.InitPeerId,
      goThrough.map { case (n, t) =>
        VarRaw(n, t)
      }
    ).wrap(
      op
    )

  override val onInitPeer: OnModel =
    OnModel(
      LiteralModel.fromRaw(ValueRaw.InitPeerId),
      goThrough.map { case (n, t) =>
        VarModel(n, t, Chain.empty)
      }
    )

}
