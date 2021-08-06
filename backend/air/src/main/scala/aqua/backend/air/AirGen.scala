package aqua.backend.air

import aqua.model._
import aqua.model.func.Call
import aqua.model.func.resolved._
import aqua.types.StreamType
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import wvlet.log.LogSupport

sealed trait AirGen {
  def generate: Air

}

object AirGen extends LogSupport {

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

  private def folder(op: ResolvedOp, ops: Chain[AirGen]): Eval[AirGen] =
    op match {
//      case mt: MetaTag =>
//        folder(mt.op, ops).map(ag => mt.comment.fold(ag)(CommentGen(_, ag)))
      case SeqRes =>
        Eval later ops.toList.reduceLeftOption(SeqGen).getOrElse(NullGen)
      case ParRes =>
        Eval later (ops.toList match {
          case o :: Nil => ParGen(o, NullGen)
          case _ =>
            ops.toList.reduceLeftOption(ParGen).getOrElse {
              warn("ParRes with no children converted to Null")
              NullGen
            }
        })
      case XorRes =>
        Eval later (ops.toList match {
          case o :: Nil => XorGen(o, NullGen)
          case _ =>
            ops.toList.reduceLeftOption(XorGen).getOrElse {
              warn("XorRes with no children converted to Null")
              NullGen
            }
        })

      case NextRes(item) =>
        Eval later NextGen(item)
      case MatchMismatchRes(left, right, shouldMatch) =>
        Eval later MatchMismatchGen(
          valueToData(left),
          valueToData(right),
          shouldMatch,
          opsToSingle(ops)
        )

      case FoldRes(item, iterable) =>
        Eval later ForGen(valueToData(iterable), item, opsToSingle(ops))
      case CallServiceRes(serviceId, funcName, CallRes(args, exportTo), peerId) =>
        Eval.later(
          ServiceCallGen(
            valueToData(peerId),
            valueToData(serviceId),
            funcName,
            args.map(valueToData),
            exportTo.map {
              case Call.Export(name, _: StreamType) => "$" + name
              case Call.Export(name, _) => name
            }
          )
        )

      case _: NoAir =>
        Eval later NullGen

    }

  def apply(op: Cofree[Chain, ResolvedOp]): AirGen =
    Cofree
      .cata[Chain, ResolvedOp, AirGen](op)(folder)
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
