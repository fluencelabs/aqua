package aqua.res

import aqua.types.{ArrayType, StreamType}
import cats.Eval
import cats.data.{Chain, NonEmptyList}
import cats.free.Cofree
import aqua.raw.value.{LiteralRaw, ValueRaw}
import aqua.model.*

// TODO docs
object MakeRes {
  type Res = ResolvedOp.Tree

  val nilTail: Eval[Chain[Res]] = Eval.now(Chain.empty)
  val op: ValueModel = LiteralModel.fromRaw(LiteralRaw.quote("op"))

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

  def canon(onPeer: ValueModel, operand: ValueModel, target: CallModel.Export): Res =
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

  private def orInit(currentPeerId: Option[ValueModel]): ValueModel =
    currentPeerId.getOrElse(initPeerId)

  def resolve(
    currentPeerId: Option[ValueModel],
    i: Int
  ): PartialFunction[OpModel, Res] = {
    case SeqModel => leaf(SeqRes)
    case _: OnModel => leaf(SeqRes)
    case MatchMismatchModel(a, b, s) =>
      leaf(MatchMismatchRes(a, b, s))
    case ForModel(item, iter) => leaf(FoldRes(item, iter))
    case RestrictionModel(item, isStream) => leaf(RestrictionRes(item, isStream))
    case ParModel | DetachModel => leaf(ParRes)
    case XorModel => leaf(XorRes)
    case NextModel(item) => leaf(NextRes(item))
    case PushToStreamModel(operand @ VarModel(_, StreamType(st), _), exportTo) =>
      val tmpName = s"push-to-stream-$i"
      // wrap (
      //  RestrictionRes(tmpName, isStream = false),
      seq(
        canon(
          orInit(currentPeerId),
          operand,
          CallModel.Export(tmpName, ArrayType(st))
        ),
        leaf(ApRes(VarModel(tmpName, ArrayType(st), Chain.empty), exportTo))
      )
    // )
    case PushToStreamModel(operand, exportTo) =>
      leaf(ApRes(operand, exportTo))

    case CanonicalizeModel(operand, exportTo) =>
      canon(
        orInit(currentPeerId),
        operand,
        exportTo
      )
    case FlattenModel(operand, assignTo) =>
      leaf(ApRes(operand, CallModel.Export(assignTo, operand.`type`)))
    case JoinModel(operands) =>
      join(orInit(currentPeerId), operands)
    case CallServiceModel(serviceId, funcName, CallModel(args, exportTo)) =>
      leaf(
        CallServiceRes(
          serviceId,
          funcName,
          CallRes(args, exportTo.headOption),
          orInit(currentPeerId)
        )
      )
  }
}
