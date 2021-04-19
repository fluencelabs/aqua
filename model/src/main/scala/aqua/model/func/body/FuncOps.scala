package aqua.model.func.body

import aqua.model.{LiteralModel, ValueModel}
import aqua.model.func.Call
import cats.data.Chain

object FuncOps {

  def noop(peerId: ValueModel): FuncOp =
    FuncOp.leaf(CallServiceTag(LiteralModel("\"op\""), "identity", Call(Nil, None), Some(peerId)))

  def callService(srvId: ValueModel, funcName: String, call: Call): FuncOp =
    FuncOp.leaf(
      CallServiceTag(
        srvId,
        funcName,
        call
      )
    )

  def onVia(on: ValueModel, via: Chain[ValueModel], wrap: FuncOp): FuncOp =
    FuncOp.wrap(
      OnTag(on, via),
      wrap
    )

  def seq(op1: FuncOp, ops: FuncOp*): FuncOp =
    FuncOp.node(SeqTag, Chain.fromSeq(op1 +: ops))

  def xor(left: FuncOp, right: FuncOp): FuncOp =
    FuncOp.node(XorTag, Chain(left, right))
}
