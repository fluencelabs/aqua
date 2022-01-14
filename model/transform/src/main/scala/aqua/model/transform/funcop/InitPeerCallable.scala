package aqua.model.transform.funcop

import aqua.model.{CallModel, CallServiceModel, LiteralModel, OnModel, OpModel, ValueModel}
import aqua.raw.value.ValueRaw
import cats.data.Chain

sealed trait InitPeerCallable {
  def transform(op: OpModel.Tree): OpModel.Tree

  def makeCall(serviceId: ValueModel, funcName: String, call: CallModel): OpModel.Tree =
    transform(CallServiceModel(serviceId, funcName, call).leaf)

  def service(serviceId: ValueModel): (String, CallModel) => OpModel.Tree =
    makeCall(serviceId, _, _)
}

case class InitViaRelayCallable(goThrough: Chain[ValueModel]) extends InitPeerCallable {

  // Get to init user through a relay
  override def transform(op: OpModel.Tree): OpModel.Tree =
    OnModel(LiteralModel.fromRaw(ValueRaw.InitPeerId), goThrough).wrap(op)

}
