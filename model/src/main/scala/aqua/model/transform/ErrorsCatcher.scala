package aqua.model.transform

import aqua.model.{LiteralModel, ValueModel}
import aqua.model.func.Call
import aqua.model.func.body.{FuncOp, FuncOps}
import aqua.types.ScalarType.string

case class ErrorsCatcher(
  enabled: Boolean,
  serviceId: ValueModel,
  funcName: String,
  callable: InitPeerCallable
) {

  def transform(op: FuncOp): FuncOp =
    if (enabled)
      FuncOps.xor(
        op,
        callable.makeCall(
          serviceId,
          funcName,
          ErrorsCatcher.lastErrorCall
        )
      )
    else op

}

object ErrorsCatcher {
  // TODO not a string
  val lastErrorArg: Call.Arg = Call.Arg(LiteralModel("%last_error%"), string)

  val lastErrorCall: Call = Call(
    lastErrorArg :: Nil,
    None
  )
}
