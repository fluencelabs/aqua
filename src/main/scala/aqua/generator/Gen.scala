package aqua.generator

import aqua.model.{
  Call,
  CallServiceTag,
  CoalgebraTag,
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
  self =>
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
        case (OnTag(peerId), ops) =>
          // TODO should be resolved
          Eval later opsToSingle(
            ops
          ) //.wrap(ctx => (ctx.copy(peerId = valueToData(peerId)), _.copy(peerId = ctx.peerId)))
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

            private val opWrap = opsToSingle(ops)
//              .wrap(ctx =>
//              (if (ctx.vars(item)) {
//                 val vn = item + ctx.instrCounter
//                 ctx.copy(vars = ctx.vars + vn, data = ctx.data.updated(item, DataView.Variable(vn)))
//               } else
//                 ctx.copy(vars = ctx.vars + item, data = ctx.data.updated(item, DataView.Variable(item)))) -> (cu =>
//                cu.copy(data = cu.data - item)
//              )
//            )

            override def generate: Air = {
              val varName =
//                if (ctx.vars(item))
//                  item + ctx.instrCounter
//                else
                item

              val iterData = valueToData(iterable)

              val resAir = opWrap.generate

              Air.Fold(iterData, varName, resAir)
            }
          }
        case (CallServiceTag(serviceId, funcName, Call(args, exportTo)), _) =>
          Eval.later(
            ServiceCallGen(valueToData(serviceId), funcName, args.map(_._1).map(valueToData), exportTo)
          )
        // TODO: coalgebra should be already resolved!
        case (CoalgebraTag(_, funcName, Call(args, exportTo)), _) =>
          ???
//          Eval.later(
//            new AirGen {
//
//              override def generate(ctx: AirContext): (AirContext, Air) =
//                ctx.arrows(funcName).toCallGen(args.map(_._1).map(valueToData), exportTo).generate(ctx)
//            }
//          )

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
  srvId: DataView,
  fnName: String,
  args: List[DataView],
  result: Option[String]
) extends AirGen {

  override def generate: Air = {

    // TODO get init peer id
    Air.Call(
      Triplet.Full(DataView.InitPeerId, srvId, fnName),
      args,
      result
    )
  }
}

case class ParGen(left: AirGen, right: AirGen) extends AirGen {

  override def generate: Air =
    Air.Par(left.generate, right.generate)
}

case class XorGen(left: AirGen, right: AirGen) extends AirGen {

  override def generate: Air =
    Air.Xor(left.generate, right.generate)
}
