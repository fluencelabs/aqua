package aqua.res

import aqua.types.{ArrayType, CanonStreamType, StreamType}
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

  private def isNillLiteral(vm: ValueModel): Boolean = vm match {
    case LiteralModel(value, t) if value == ValueRaw.Nil.value && t == ValueRaw.Nil.`type` => true
    case _ => false
  }

  def resolve(
    currentPeerId: Option[ValueModel],
    i: Int
  ): PartialFunction[OpModel, ResolvedOp.Tree] = {
    case SeqModel | _: OnModel | _: ApplyTopologyModel => SeqRes.leaf
    case MatchMismatchModel(a, b, s) =>
      MatchMismatchRes(a, b, s).leaf
    case ForModel(item, iter) if !isNillLiteral(iter) => FoldRes(item, iter).leaf
    case RestrictionModel(item, isStream) => RestrictionRes(item, isStream).leaf
    case ParModel | DetachModel => ParRes.leaf
    case XorModel => XorRes.leaf
    case NextModel(item) => NextRes(item).leaf
    case PushToStreamModel(operand @ VarModel(_, t@StreamType(st), _), exportTo) =>
      val properties = operand.properties
      if (properties.isEmpty) {
        ApRes(VarModel(operand.name, t, Chain.empty), exportTo).leaf
      } else {
          val tmpName = s"push-to-stream-$i"
          SeqRes.wrap(
          CanonRes(
            operand.copy(properties = Chain.empty),
            orInit(currentPeerId),
            CallModel.Export(tmpName, CanonStreamType(st))
          ).leaf,
          ApRes(VarModel(tmpName, CanonStreamType(st), properties), exportTo).leaf
        )
      }
    case PushToStreamModel(operand, exportTo) =>
      ApRes(operand, exportTo).leaf

    case CanonicalizeModel(operand, exportTo, withOp) =>
      if (withOp)
        canon(orInit(currentPeerId), operand, exportTo)
      else
        CanonRes(
          operand,
          orInit(currentPeerId),
          exportTo
        ).leaf

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

    case NullModel =>
      NullRes.leaf

  }
}
