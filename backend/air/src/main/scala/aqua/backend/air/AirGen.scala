package aqua.backend.air

import aqua.model._
import aqua.model.func.Call
import aqua.model.func.body._
import aqua.types.StreamType
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

  def wrapMatchWithXor(ag: AirGen): AirGen = ag match {
    case mmg: MatchMismatchGen =>
      XorGen(mmg, NullGen)
    case g => g
  }

  def apply(op: Cofree[Chain, OpTag]): AirGen =
    Cofree
      .cata[Chain, OpTag, AirGen](op) {
        case (SeqTag, ops) =>
          Eval later ops.map(wrapMatchWithXor).toList.reduceLeftOption(SeqGen).getOrElse(NullGen)
        case (ParTag, ops) =>
          Eval later ops.map(wrapMatchWithXor).toList.reduceLeftOption(ParGen).getOrElse(NullGen)
        case (XorTag, ops) =>
          Eval later ops.toList.reduceLeftOption(XorGen).getOrElse(NullGen)
        case (NextTag(item), _) =>
          Eval later NextGen(item)
        case (MatchMismatchTag(left, right, shouldMatch), ops) =>
          Eval later MatchMismatchGen(
            valueToData(left),
            valueToData(right),
            shouldMatch,
            opsToSingle(ops)
          )

        case (ForTag(item, iterable), ops) =>
          Eval later ForGen(valueToData(iterable), item, opsToSingle(ops.map(wrapMatchWithXor)))
        case (CallServiceTag(serviceId, funcName, Call(args, exportTo), peerId), _) =>
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

        case (CallArrowTag(funcName, _), _) =>
          // TODO: should be already resolved & removed from tree
          println(
            Console.RED + s"Unresolved arrow in AirGen: $funcName" + Console.RESET
          )
          Eval later NullGen

        case (OnTag(_, _), ops) =>
          // TODO should be resolved
          Eval later opsToSingle(
            ops
          )
        case (XorParTag(opsx, opsy), _) =>
          // TODO should be resolved
          println(
            Console.RED + "XorParTag reached AirGen, most likely it's an error" + Console.RESET
          )
          Eval later opsToSingle(
            Chain(apply(opsx.tree), apply(opsy.tree))
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
