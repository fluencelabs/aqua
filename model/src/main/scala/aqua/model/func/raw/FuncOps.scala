package aqua.model.func.raw

import aqua.model.func.Call
import aqua.model.{LiteralModel, ValueModel}
import cats.data.Chain
import cats.free.Cofree

object FuncOps {

  def noop: FuncOp =
    FuncOp.leaf(CallServiceTag(LiteralModel.quote("op"), "identity", Call(Nil, None)))

  def identity(what: ValueModel, to: Call.Export): FuncOp =
    FuncOp.leaf(
      CallServiceTag(LiteralModel.quote("op"), "identity", Call(what :: Nil, Some(to)))
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
      CallArrowTag(
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
    if (ops.length == 1) ops.head
    else
      FuncOp.node(
        SeqTag,
        Chain
          .fromSeq(ops.flatMap {
            case FuncOp(Cofree(SeqTag, subOps)) => subOps.value.toList
            case FuncOp(cof) => cof :: Nil
          })
          .map(FuncOp(_))
      )

  def par(ops: FuncOp*): FuncOp =
    if (ops.length == 1) ops.head
    else
      FuncOp.node(
        ParTag,
        Chain
          .fromSeq(ops.flatMap {
            case FuncOp(Cofree(ParTag, subOps)) => subOps.value.toList
            case FuncOp(cof) => cof :: Nil
          })
          .map(FuncOp(_))
      )

  def co(ops: FuncOp*): FuncOp =
    FuncOp.wrap(ParTag.Detach, seq(ops: _*))

  def xor(left: FuncOp, right: FuncOp): FuncOp =
    FuncOp.node(XorTag, Chain(left, right))

  def fold(item: String, iter: ValueModel, op: FuncOp): FuncOp =
    FuncOp.wrap(
      ForTag(item, iter),
      op
    )

  def next(item: String): FuncOp =
    FuncOp.leaf(NextTag(item))

  lazy val empty: FuncOp =
    FuncOp.leaf(EmptyTag)

}
