package aqua.res

import aqua.model.*
import aqua.raw.value.{LiteralRaw, ValueRaw}
import aqua.types.*
import aqua.types.{ArrayType, CanonStreamType, StreamType}

import cats.Eval
import cats.data.{Chain, NonEmptyList}
import cats.free.Cofree

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
    val streamName = "-hop-"
    val canonName = "-hopc-"
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
    case ForModel(item, iter, mode) if !isNillLiteral(iter) =>
      val modeRes = mode match {
        case ForModel.Mode.Null => FoldRes.Mode.Null
        case ForModel.Mode.Never => FoldRes.Mode.Never
      }

      FoldRes(item, iter, modeRes).leaf
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
    case InsertKeyValueModel(key, value, assignTo, assignToType) =>
      ApStreamMapRes(key, value, CallModel.Export(assignTo, assignToType)).leaf
    case FlattenModel(operand @ VarModel(_, CanonStreamType(el), _), assignTo) =>
      ApRes(operand, CallModel.Export(assignTo, ArrayType(el))).leaf
    case FlattenModel(operand, assignTo) =>
      ApRes(operand, CallModel.Export(assignTo, operand.`type`)).leaf
    case FailModel(value) =>
      FailRes(value).leaf
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
