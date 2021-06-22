package aqua.model.func.resolved

import aqua.model.func.Call
import aqua.model.{LiteralModel, ValueModel}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree

object MakeRes {
  val nilTail: Eval[Chain[Cofree[Chain, ResolvedOp]]] = Eval.now(Chain.empty)
  type Cof = Cofree[Chain, ResolvedOp]
  def leaf(op: ResolvedOp): Cof = Cofree[Chain, ResolvedOp](op, nilTail)

  def next(item: String): Cof =
    leaf(NextRes(item))

  def seq(first: Cof, second: Cof, more: Cof*): Cof =
    Cofree[Chain, ResolvedOp](SeqRes, Eval.later(first +: second +: Chain.fromSeq(more)))

  def par(first: Cof, second: Cof, more: Cof*): Cof =
    Cofree[Chain, ResolvedOp](ParRes, Eval.later(first +: second +: Chain.fromSeq(more)))

  def xor(first: Cof, second: Cof): Cof =
    Cofree[Chain, ResolvedOp](XorRes, Eval.later(Chain(first, second)))

  def fold(item: String, iter: ValueModel, body: Cof): Cof =
    Cofree[Chain, ResolvedOp](FoldRes(item, iter), Eval.now(Chain.one(body)))

  def noop(onPeer: ValueModel): Cof =
    leaf(CallServiceRes(LiteralModel.quote("op"), "noop", Call(Nil, None), onPeer))
}
