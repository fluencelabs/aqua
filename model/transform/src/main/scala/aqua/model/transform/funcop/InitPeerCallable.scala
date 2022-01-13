package aqua.model.transform.funcop

import aqua.raw.ops.{Call, FuncOp, FuncOps}
import aqua.raw.value.ValueRaw
import cats.data.Chain

sealed trait InitPeerCallable {
  def transform(op: FuncOp): FuncOp

  def makeCall(serviceId: ValueRaw, funcName: String, call: Call): FuncOp =
    transform(FuncOps.callService(serviceId, funcName, call))

  def service(serviceId: ValueRaw): (String, Call) => FuncOp = makeCall(serviceId, _, _)
}

case class InitViaRelayCallable(goThrough: Chain[ValueRaw]) extends InitPeerCallable {

  // Get to init user through a relay
  override def transform(op: FuncOp): FuncOp =
    FuncOps.onVia(ValueRaw.InitPeerId, goThrough, op)

}
