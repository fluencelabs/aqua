package aqua.res

import aqua.types.{ArrayType, StreamType}
import cats.Eval
import cats.data.{Chain, NonEmptyList}
import cats.free.Cofree
import aqua.raw.value.{LiteralRaw, ValueRaw}
import aqua.model.*

// TODO docs
object MakeRes {
  val op: ValueModel = LiteralModel.fromRaw(LiteralRaw.quote("op"))

  def noop(onPeer: ValueModel, log: String = null): ResolvedOp.Tree =
    CallServiceRes(
      op,
      "noop",
      CallRes(
        Option(log).filter(_ == "").map(LiteralRaw.quote).map(LiteralModel.fromRaw).toList,
        None
      ),
      onPeer
    ).leaf

  def canon(onPeer: ValueModel, operand: ValueModel, target: CallModel.Export): ResolvedOp.Tree =
    CallServiceRes(
      op,
      "identity",
      CallRes(operand :: Nil, Some(target)),
      onPeer
    ).leaf

  def join(onPeer: ValueModel, operands: NonEmptyList[ValueModel]): ResolvedOp.Tree =
    CallServiceRes(
      op,
      "noop",
      CallRes(operands.toList, None),
      onPeer
    ).leaf

  private val initPeerId = ValueModel.fromRaw(ValueRaw.InitPeerId)

  private def orInit(currentPeerId: Option[ValueModel]): ValueModel =
    currentPeerId.getOrElse(initPeerId)

  def resolve(
    currentPeerId: Option[ValueModel],
    i: Int
  ): PartialFunction[OpModel, ResolvedOp.Tree] = {
    case SeqModel => SeqRes.leaf
    case _: OnModel => SeqRes.leaf
    case MatchMismatchModel(a, b, s) =>
      MatchMismatchRes(a, b, s).leaf
    case ForModel(item, iter) => FoldRes(item, iter).leaf
    case RestrictionModel(item, isStream) => RestrictionRes(item, isStream).leaf
    case ParModel | DetachModel => ParRes.leaf
    case XorModel => XorRes.leaf
    case NextModel(item) => NextRes(item).leaf
    case PushToStreamModel(operand @ VarModel(_, StreamType(st), _), exportTo) =>
      val tmpName = s"push-to-stream-$i"
      // wrap (
      //  RestrictionRes(tmpName, isStream = false),
      SeqRes.wrap(
        canon(
          orInit(currentPeerId),
          operand,
          CallModel.Export(tmpName, ArrayType(st))
        ),
        ApRes(VarModel(tmpName, ArrayType(st), Chain.empty), exportTo).leaf
      )
    // )
    case PushToStreamModel(operand, exportTo) =>
      ApRes(operand, exportTo).leaf

    case CanonicalizeModel(operand, exportTo) =>
      canon(
        orInit(currentPeerId),
        operand,
        exportTo
      )
    case FlattenModel(operand, assignTo) =>
      ApRes(operand, CallModel.Export(assignTo, operand.`type`)).leaf
    case JoinModel(operands) =>
      join(orInit(currentPeerId), operands)
    case CallServiceModel(serviceId, funcName, CallModel(args, exportTo)) =>
      CallServiceRes(
        serviceId,
        funcName,
        CallRes(args, exportTo.headOption),
        orInit(currentPeerId)
      ).leaf

  }
}
