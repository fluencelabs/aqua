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
import aqua.raw.ops.{Call, CallServiceTag, FuncOp, FuncOps}
import aqua.raw.value.{ValueRaw, VarRaw}
import cats.data.Chain
import aqua.types.Type

sealed trait InitPeerCallable extends PreTransform {

  def onInitPeer: OnModel

  def makeCall(serviceId: ValueRaw, funcName: String, call: Call): FuncOp =
    transform(FuncOp.leaf(CallServiceTag(serviceId, funcName, call)))

  def service(serviceId: ValueRaw): (String, Call) => FuncOp =
    makeCall(serviceId, _, _)
}

// TODO: refactor goThrough into some supertype of VarRaw and VarModel with no lambda
case class InitViaRelayCallable(goThrough: Chain[(String, Type)]) extends InitPeerCallable {

  // Get to init user through a relay
  override def transform(op: FuncOp): FuncOp =
    FuncOps.onVia(
      ValueRaw.InitPeerId,
      goThrough.map { case (n, t) =>
        VarRaw(n, t)
      },
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
