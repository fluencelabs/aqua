package aqua.model.transform.res

import aqua.model.func.Call
import aqua.model.func.raw.*
import aqua.model.transform.topology.Topology.Res
import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.types.{ArrayType, StreamType}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree

object MakeRes {
  val nilTail: Eval[Chain[Res]] = Eval.now(Chain.empty)

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

  def canon(onPeer: ValueModel, operand: ValueModel, target: Call.Export): Res =
    leaf(
      CallServiceRes(
        LiteralModel.quote("op"),
        "identity",
        CallRes(operand :: Nil, Some(target)),
        onPeer
      )
    )

  def resolve(
    currentPeerId: Option[ValueModel],
    i: Int
  ): PartialFunction[RawTag, Res] = {
    case SeqTag => leaf(SeqRes)
    case _: OnTag => leaf(SeqRes)
    case MatchMismatchTag(a, b, s) => leaf(MatchMismatchRes(a, b, s))
    case ForTag(item, iter) => leaf(FoldRes(item, iter))
    case ParTag | ParTag.Detach => leaf(ParRes)
    case XorTag | XorTag.LeftBiased => leaf(XorRes)
    case NextTag(item) => leaf(NextRes(item))
    case PushToStreamTag(operand, exportTo) =>
      operand.`type` match {
        case StreamType(st) =>
          val tmpName = s"push-to-stream-$i"
          seq(
            canon(
              currentPeerId
                .getOrElse(LiteralModel.initPeerId),
              operand,
              Call.Export(tmpName, ArrayType(st))
            ),
            leaf(ApRes(VarModel(tmpName, ArrayType(st)), exportTo))
          )
        case _ =>
          leaf(ApRes(operand, exportTo))
      }
    case CallServiceTag(serviceId, funcName, Call(args, exportTo)) =>
      leaf(
        CallServiceRes(
          serviceId,
          funcName,
          CallRes(args, exportTo.headOption),
          currentPeerId
            .getOrElse(LiteralModel.initPeerId)
        )
      )
  }
}
