package aqua.model.func.resolved

import aqua.model.func.Call
import aqua.model.topology.Topology.Res
import aqua.model.{LiteralModel, ValueModel}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree

object MakeRes {
  val nilTail: Eval[Chain[Cofree[Chain, ResolvedOp]]] = Eval.now(Chain.empty)

  def leaf(op: ResolvedOp): Res = Cofree[Chain, ResolvedOp](op, nilTail)

  def next(item: String): Res =
    leaf(NextRes(item))

  def seq(first: Res, second: Res, more: Res*): Res =
    Cofree[Chain, ResolvedOp](SeqRes, Eval.later(first +: second +: Chain.fromSeq(more)))

  def par(first: Res, second: Res, more: Res*): Res =
    Cofree[Chain, ResolvedOp](ParRes, Eval.later(first +: second +: Chain.fromSeq(more)))

  def xor(first: Res, second: Res): Res =
    Cofree[Chain, ResolvedOp](XorRes, Eval.later(Chain(first, second)))

  def fold(item: String, iter: ValueModel, body: Res*): Res =
    Cofree[Chain, ResolvedOp](FoldRes(item, iter), Eval.now(Chain.fromSeq(body)))

  def noop(onPeer: ValueModel): Res =
    leaf(CallServiceRes(LiteralModel.quote("op"), "noop", Call(Nil, None), onPeer))
}
