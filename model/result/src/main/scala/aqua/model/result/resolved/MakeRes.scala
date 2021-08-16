package aqua.model.result.resolved

import aqua.model.func.Call
import aqua.model.func.raw.{
  CallServiceTag,
  ForTag,
  MatchMismatchTag,
  NextTag,
  OnTag,
  ParTag,
  RawTag,
  SeqTag,
  XorTag
}
import aqua.model.result.topology.Topology.Res
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

  def fold(item: String, iter: ValueModel, body: Res): Res =
    Cofree[Chain, ResolvedOp](FoldRes(item, iter), Eval.now(Chain.one(body)))

  def noop(onPeer: ValueModel): Res =
    leaf(CallServiceRes(LiteralModel.quote("op"), "noop", CallRes(Nil, None), onPeer))

  def resolve(
    currentPeerId: Option[ValueModel]
  ): PartialFunction[RawTag, ResolvedOp] = {
    case SeqTag => SeqRes
    case _: OnTag => SeqRes
    case MatchMismatchTag(a, b, s) => MatchMismatchRes(a, b, s)
    case ForTag(item, iter) => FoldRes(item, iter)
    case ParTag | ParTag.Detach => ParRes
    case XorTag | XorTag.LeftBiased => XorRes
    case NextTag(item) => NextRes(item)
    case CallServiceTag(serviceId, funcName, Call(args, exportTo)) =>
      CallServiceRes(
        serviceId,
        funcName,
        CallRes(args, exportTo.headOption),
        currentPeerId
          .getOrElse(LiteralModel.initPeerId)
      )
  }
}
