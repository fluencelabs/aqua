package aqua.model.transform.res

import aqua.model.transform.topology.Topology.Res
import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.raw.ops.*
import aqua.raw.value.{LiteralRaw, ValueRaw}
import aqua.types.{ArrayType, StreamType}
import cats.Eval
import cats.data.{Chain, NonEmptyList}
import cats.free.Cofree

// TODO docs
object MakeRes {
  val nilTail: Eval[Chain[Res]] = Eval.now(Chain.empty)
  val op: ValueModel = ValueModel.fromRaw(LiteralRaw.quote("op"))

  def leaf(op: ResolvedOp): Res = Cofree[Chain, ResolvedOp](op, nilTail)

  def next(item: String): Res =
    leaf(NextRes(item))

  def wrap(op: ResolvedOp, internal: Res): Res =
    Cofree[Chain, ResolvedOp](op, Eval.now(Chain.one(internal)))

  def seq(first: Res, second: Res, more: Res*): Res =
    Cofree[Chain, ResolvedOp](SeqRes, Eval.later(first +: second +: Chain.fromSeq(more)))

  def par(first: Res, more: Res*): Res =
    Cofree[Chain, ResolvedOp](ParRes, Eval.later(first +: Chain.fromSeq(more)))

  def xor(first: Res, second: Res): Res =
    Cofree[Chain, ResolvedOp](XorRes, Eval.later(Chain(first, second)))

  def fold(item: String, iter: ValueModel, body0: Res, body: Res*): Res =
    Cofree[Chain, ResolvedOp](
      FoldRes(item, iter),
      Eval.now(Chain.one(body0) ++ Chain.fromSeq(body))
    )

  def noop(onPeer: ValueModel): Res =
    leaf(CallServiceRes(op, "noop", CallRes(Nil, None), onPeer))

  def canon(onPeer: ValueModel, operand: ValueModel, target: Call.Export): Res =
    leaf(
      CallServiceRes(
        op,
        "identity",
        CallRes(operand :: Nil, Some(target)),
        onPeer
      )
    )

  def join(onPeer: ValueModel, operands: NonEmptyList[ValueModel]): Res =
    leaf(
      CallServiceRes(
        op,
        "noop",
        CallRes(operands.toList, None),
        onPeer
      )
    )

  private val initPeerId = ValueModel.fromRaw(ValueRaw.InitPeerId)

  private def orInit(currentPeerId: Option[ValueRaw]): ValueModel =
    currentPeerId.fold(initPeerId)(ValueModel.fromRaw)

  def resolve(
    currentPeerId: Option[ValueRaw],
    i: Int
  ): PartialFunction[RawTag, Res] = {
    case SeqTag => leaf(SeqRes)
    case _: OnTag => leaf(SeqRes)
    case MatchMismatchTag(a, b, s) =>
      leaf(MatchMismatchRes(ValueModel.fromRaw(a), ValueModel.fromRaw(b), s))
    case ForTag(item, iter) => leaf(FoldRes(item, ValueModel.fromRaw(iter)))
    case RestrictionTag(item, isStream) => leaf(RestrictionRes(item, isStream))
    case ParTag | ParTag.Detach => leaf(ParRes)
    case XorTag | XorTag.LeftBiased => leaf(XorRes)
    case NextTag(item) => leaf(NextRes(item))
    case PushToStreamTag(operand, exportTo) =>
      operand.`type` match {
        case StreamType(st) =>
          val tmpName = s"push-to-stream-$i"
          // wrap (
          //  RestrictionRes(tmpName, isStream = false),
          seq(
            canon(
              orInit(currentPeerId),
              ValueModel.fromRaw(operand),
              Call.Export(tmpName, ArrayType(st))
            ),
            leaf(ApRes(VarModel(tmpName, ArrayType(st), Chain.empty), exportTo))
          )
        // )
        case _ =>
          leaf(ApRes(ValueModel.fromRaw(operand), exportTo))
      }
    case CanonicalizeTag(operand, exportTo) =>
      canon(
        orInit(currentPeerId),
        ValueModel.fromRaw(operand),
        exportTo
      )
    case FlattenTag(operand, assignTo) =>
      leaf(ApRes(ValueModel.fromRaw(operand), Call.Export(assignTo, operand.`type`)))
    case JoinTag(operands) =>
      join(orInit(currentPeerId), operands.map(ValueModel.fromRaw))
    case CallServiceTag(serviceId, funcName, Call(args, exportTo)) =>
      leaf(
        CallServiceRes(
          ValueModel.fromRaw(serviceId),
          funcName,
          CallRes(args.map(ValueModel.fromRaw), exportTo.headOption),
          orInit(currentPeerId)
        )
      )
  }
}
