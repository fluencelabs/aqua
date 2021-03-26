package aqua.generator

import aqua.model.{
  Call,
  CallArrowTag,
  CallServiceTag,
  ForTag,
  InitPeerIdModel,
  IntoArrayModel,
  IntoFieldModel,
  LambdaModel,
  LiteralModel,
  MatchMismatchTag,
  NextTag,
  OnTag,
  OpTag,
  ParTag,
  SeqTag,
  ValueModel,
  VarModel,
  XorTag
}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree

sealed trait AirGen {
  def generate: Air

}

object AirGen {

  def lambdaToString(ls: List[LambdaModel]): String = ls match {
    case Nil => ""
    case IntoArrayModel :: tail =>
      s"[@${lambdaToString(tail)}]"
    case IntoFieldModel(field) :: tail =>
      s".$field${lambdaToString(tail)}"
  }

  def valueToData(vm: ValueModel): DataView = vm match {
    case LiteralModel(value) => DataView.StringScalar(value)
    case InitPeerIdModel => DataView.InitPeerId
    case VarModel(name, lambda) =>
      if (lambda.isEmpty) DataView.Variable(name)
      else DataView.VarLens(name, lambdaToString(lambda.toList))
  }

  def opsToSingle(ops: Chain[AirGen]): AirGen = ops.toList match {
    case Nil => NullGen
    case h :: Nil => h
    case list => list.reduceLeft(SeqGen)
  }

  def apply(op: Cofree[Chain, OpTag]): AirGen =
    Cofree
      .cata[Chain, OpTag, AirGen](op) {

        case (SeqTag, ops) => Eval later ops.toList.reduceLeftOption(SeqGen).getOrElse(NullGen)
        case (ParTag, ops) => Eval later ops.toList.reduceLeftOption(ParGen).getOrElse(NullGen)
        case (XorTag, ops) => Eval later ops.toList.reduceLeftOption(XorGen).getOrElse(NullGen)
        case (NextTag(item), ops) =>
          Eval later new AirGen {

            override def generate: Air =
              Air.Next(item)
          }
        case (MatchMismatchTag(left, right, shouldMatch), ops) =>
          Eval later new AirGen {

            override def generate: Air = {
              val l = valueToData(left)
              val r = valueToData(right)
              val resAir = opsToSingle(ops).generate
              if (shouldMatch) Air.Match(l, r, resAir) else Air.Mismatch(l, r, resAir)
            }
          }
        case (ForTag(item, iterable), ops) =>
          Eval later new AirGen {

            override def generate: Air = {

              val iterData = valueToData(iterable)

              val resAir = opsToSingle(ops).generate

              Air.Fold(iterData, item, resAir)
            }
          }
        case (CallServiceTag(serviceId, funcName, Call(args, exportTo), peerId), _) =>
          Eval.later(
            ServiceCallGen(
              peerId.map(valueToData).getOrElse(DataView.InitPeerId),
              valueToData(serviceId),
              funcName,
              args.map(_._1).map(valueToData),
              exportTo
            )
          )

        case (CallArrowTag(_, funcName, Call(args, exportTo)), ops) =>
          // TODO: should be already resolved & removed from tree
          Eval later opsToSingle(
            ops
          )

        case (OnTag(_, _), ops) =>
          // TODO should be resolved
          Eval later opsToSingle(
            ops
          )

      }
      .value
}

case object NullGen extends AirGen {
  override def generate: Air = Air.Null
}

case class SeqGen(left: AirGen, right: AirGen) extends AirGen {

  override def generate: Air =
    Air.Seq(left.generate, right.generate)

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
