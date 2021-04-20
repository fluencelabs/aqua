package aqua.model.func.body

import aqua.model.{LiteralModel, ValueModel}
import aqua.model.func.Call
import cats.data.Chain
import cats.free.Cofree

object FuncOps {

  def noop(peerId: ValueModel): FuncOp =
    FuncOp.leaf(CallServiceTag(LiteralModel("\"op\""), "identity", Call(Nil, None), Some(peerId)))

  def assign(arg: Call.Arg, name: String): FuncOp =
    FuncOp.leaf(
      CallServiceTag(LiteralModel("\"op\""), "identity", Call(List(arg), Some(name)), None)
    )

  def callService(srvId: ValueModel, funcName: String, call: Call): FuncOp =
    FuncOp.leaf(
      CallServiceTag(
        srvId,
        funcName,
        call
      )
    )

  def callArrow(funcName: String, call: Call): FuncOp =
    FuncOp.leaf(
      CallFunctionTag(
        funcName,
        call
      )
    )

  def onVia(on: ValueModel, via: Chain[ValueModel], wrap: FuncOp): FuncOp =
    FuncOp.wrap(
      OnTag(on, via),
      wrap
    )

  def seq(ops: FuncOp*): FuncOp =
    FuncOp.node(
      SeqTag,
      Chain
        .fromSeq(ops.flatMap {
          case FuncOp(Cofree(SeqTag, subOps)) => subOps.value.toList
          case FuncOp(cof) => cof :: Nil
        })
        .map(FuncOp(_))
    )

  def xor(left: FuncOp, right: FuncOp): FuncOp =
    FuncOp.node(XorTag, Chain(left, right))
}
