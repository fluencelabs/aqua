package aqua.res

import aqua.types.{ArrayType, CanonStreamType, StreamType}
import cats.Eval
import cats.data.{Chain, NonEmptyList}
import cats.free.Cofree
import aqua.raw.value.{LiteralRaw, ValueRaw}
import aqua.model.*
import aqua.types.*

/**
 * Helpers for translating [[OpModel]] to [[ResolvedOp]]
 */
object MakeRes {

  /**
   * Make topology hop to peer
   *
   * @param onPeer peer to make hop to
   * @return [[ResolvedOp.Tree]] corresponsing to a hop
   */
  def hop(onPeer: ValueModel): ResolvedOp.Tree = {
    // Those names can't be produced from compilation
    // so they are safe to use
    val streamName = "-ephemeral-stream-"
    val canonName = "-ephemeral-canon-"
    val elementType = BottomType
    val streamType = StreamType(elementType)
    val canonType = CanonStreamType(elementType)

    RestrictionRes(streamName, streamType).wrap(
      RestrictionRes(canonName, canonType).wrap(
        CanonRes(
          operand = VarModel(streamName, streamType),
          peerId = onPeer,
          exportTo = CallModel.Export(canonName, canonType)
        ).leaf
      )
    )
  }

  def resolve(
    currentPeerId: Option[ValueModel],
    i: Int
  ): PartialFunction[OpModel, ResolvedOp.Tree] = {
    case SeqModel | _: OnModel | _: ApplyTopologyModel => SeqRes.leaf
    case MatchMismatchModel(a, b, s) =>
      MatchMismatchRes(a, b, s).leaf
    case ForModel(item, iter, mode) if !isNillLiteral(iter) => FoldRes(item, iter, mode).leaf
    case RestrictionModel(item, itemType) => RestrictionRes(item, itemType).leaf
    case DetachModel => ParRes.leaf
    case ParModel => ParRes.leaf
    case XorModel => XorRes.leaf
    case NextModel(item) => NextRes(item).leaf
    case PushToStreamModel(operand @ VarModel(_, StreamType(st), _), exportTo) =>
      val tmpName = s"push-to-stream-$i"
      val properties = operand.properties
      SeqRes.wrap(
        CanonRes(
          operand.copy(properties = Chain.empty),
          orInit(currentPeerId),
          CallModel.Export(tmpName, CanonStreamType(st))
        ).leaf,
        ApRes(VarModel(tmpName, CanonStreamType(st), properties), exportTo).leaf
      )
    case PushToStreamModel(operand, exportTo) =>
      ApRes(operand, exportTo).leaf

    case CanonicalizeModel(operand, exportTo) =>
      CanonRes(
        operand,
        orInit(currentPeerId),
        exportTo
      ).leaf
    case FlattenModel(operand @ VarModel(_, CanonStreamType(el), _), assignTo) =>
      ApRes(operand, CallModel.Export(assignTo, ArrayType(el))).leaf
    case FlattenModel(operand, assignTo) =>
      ApRes(operand, CallModel.Export(assignTo, operand.`type`)).leaf
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

  private val initPeerId = ValueModel.fromRaw(ValueRaw.InitPeerId)

  private def orInit(currentPeerId: Option[ValueModel]): ValueModel =
    currentPeerId.getOrElse(initPeerId)

  private def isNillLiteral(vm: ValueModel): Boolean = vm match {
    case LiteralModel(value, t) if value == ValueRaw.Nil.value && t == ValueRaw.Nil.`type` => true
    case _ => false
  }
}
