package aqua.backend.air

import aqua.model._
import aqua.model.func.Call
import aqua.model.func.body._
import aqua.types.StreamType
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import wvlet.log.Logger

sealed trait AirGen {
  def generate: Air

}

object AirGen {

  private val logger = Logger.of[AirGen.type]
  import logger._

  def lambdaToString(ls: List[LambdaModel]): String = ls match {
    case Nil => ""
    case IntoArrayModel(_) :: tail =>
      s"[@${lambdaToString(tail)}]"
    case IntoFieldModel(field, _) :: tail =>
      s".$field${lambdaToString(tail)}"
    case IntoIndexModel(idx, _) :: tail =>
      s".[$idx]${lambdaToString(tail)}"
  }

  def valueToData(vm: ValueModel): DataView = vm match {
    case LiteralModel(value, _) => DataView.StringScalar(value)
    case VarModel(name, t, lambda) =>
      val n = t match {
        case _: StreamType => "$" + name
        case _ => name
      }
      if (lambda.isEmpty) DataView.Variable(n)
      else DataView.VarLens(n, lambdaToString(lambda.toList))
  }

  def opsToSingle(ops: Chain[AirGen]): AirGen = ops.toList match {
    case Nil => NullGen
    case h :: Nil => h
    case list => list.reduceLeft(SeqGen)
  }

  private def folder(op: OpTag, ops: Chain[AirGen]): Eval[AirGen] =
    op match {
      case mt: MetaTag =>
        folder(mt.op, ops).map(ag => mt.comment.fold(ag)(CommentGen(_, ag)))
      case SeqTag =>
        Eval later ops.toList.reduceLeftOption(SeqGen).getOrElse(NullGen)
      case ParTag =>
        Eval later ops.toList.reduceLeftOption(ParGen).getOrElse(NullGen)
      case XorTag =>
        Eval later ops.toList.reduceLeftOption(XorGen).getOrElse(NullGen)
      case XorTag.LeftBiased =>
        Eval later XorGen(opsToSingle(ops), NullGen)

      case NextTag(item) =>
        Eval later NextGen(item)
      case MatchMismatchTag(left, right, shouldMatch) =>
        Eval later MatchMismatchGen(
          valueToData(left),
          valueToData(right),
          shouldMatch,
          opsToSingle(ops)
        )

      case ForTag(item, iterable) =>
        Eval later ForGen(valueToData(iterable), item, opsToSingle(ops))
      case CallServiceTag(serviceId, funcName, Call(args, exportTo), peerId) =>
        Eval.later(
          ServiceCallGen(
            peerId.map(valueToData).getOrElse(DataView.InitPeerId),
            valueToData(serviceId),
            funcName,
            args.map(valueToData),
            exportTo.map {
              case Call.Export(name, _: StreamType) => "$" + name
              case Call.Export(name, _) => name
            }
          )
        )

      case CallArrowTag(funcName, _) =>
        // TODO: should be already resolved & removed from tree
        error(
          s"Unresolved arrow in AirGen: $funcName"
        )
        Eval later NullGen

      case AssignmentTag(_, _) =>
        // TODO: should be already resolved & removed from tree
        Eval later NullGen

      case OnTag(_, _) =>
        // TODO should be resolved
        Eval later opsToSingle(
          ops
        )
      case XorParTag(opsx, opsy) =>
        // TODO should be resolved
        error(
          "XorParTag reached AirGen, most likely it's an error"
        )
        Eval later opsToSingle(
          Chain(apply(opsx.tree), apply(opsy.tree))
        )

    }

  def apply(op: Cofree[Chain, OpTag]): AirGen =
    Cofree
      .cata[Chain, OpTag, AirGen](op)(folder)
      .value
}

case object NullGen extends AirGen {
  override def generate: Air = Air.Null
}

case class SeqGen(left: AirGen, right: AirGen) extends AirGen {

  override def generate: Air =
    Air.Seq(left.generate, right.generate)

}

case class CommentGen(comment: String, op: AirGen) extends AirGen {

  override def generate: Air =
    Air.Comment(comment, op.generate)
}

case class MatchMismatchGen(
  left: DataView,
  right: DataView,
  shouldMatch: Boolean,
  body: AirGen
) extends AirGen {

  override def generate: Air =
    if (shouldMatch) Air.Match(left, right, body.generate)
    else Air.Mismatch(left, right, body.generate)
}

case class ForGen(iterable: DataView, item: String, body: AirGen) extends AirGen {
  override def generate: Air = Air.Fold(iterable, item, body.generate)
}

case class NextGen(item: String) extends AirGen {
  override def generate: Air = Air.Next(item)
}

case class ServiceCallGen(
  peerId: DataView,
  srvId: DataView,
  fnName: String,
  args: List[DataView],
  result: Option[String]
) extends AirGen {

  override def generate: Air =
    Air.Call(
      Triplet.Full(peerId, srvId, fnName),
      args,
      result
    )
}

case class ParGen(left: AirGen, right: AirGen) extends AirGen {

  override def generate: Air =
    Air.Par(left.generate, right.generate)
}

case class XorGen(left: AirGen, right: AirGen) extends AirGen {

  override def generate: Air =
    Air.Xor(left.generate, right.generate)
}
