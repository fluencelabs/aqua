package aqua.model.transform

import aqua.model.{LiteralModel, ValueModel}
import aqua.model.func.Call
import aqua.model.func.body.{FuncOp, FuncOps}
import cats.data.Chain

sealed trait InitPeerCallable {
  def transform(op: FuncOp): FuncOp

  def makeCall(serviceId: ValueModel, funcName: String, call: Call): FuncOp =
    transform(FuncOps.callService(serviceId, funcName, call))

  def service(serviceId: ValueModel): (String, Call) => FuncOp = makeCall(serviceId, _, _)
}

case class InitViaRelayCallable(goThrough: Chain[ValueModel]) extends InitPeerCallable {

  // Get to init user through a relay
  override def transform(op: FuncOp): FuncOp =
    FuncOps.onVia(LiteralModel.initPeerId, goThrough, op)

}
