package aqua.generator

import aqua.model.{
  CoalgebraTag,
  ForTag,
  FuncOp,
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
  ServiceModel,
  ValueModel,
  VarModel,
  XorTag
}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import cats.syntax.functor._

sealed trait AirGen {
  self =>
  def generate(ctx: AirContext): (AirContext, Air)

  def wrap(f: AirContext => (AirContext, AirContext => AirContext)): AirGen =
    new AirGen {

      override def generate(ctx: AirContext): (AirContext, Air) = {
        val (setup, clean) = f(ctx)
        val (internal, res) = self.generate(setup.incr)
        (clean(internal).incr, res)
      }
    }
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

  def resolve(ctx: AirContext, dataView: DataView): DataView = dataView match {
    case DataView.Variable(name) => ctx.data(name)
    case DataView.Stream(name) => ctx.data(name)
    case DataView.VarLens(name, lens) =>
      ctx.data(name) match {
        case DataView.Variable(n) => DataView.VarLens(n, lens)
        case DataView.Stream(n) => DataView.VarLens(n, lens)
        case vl: DataView.VarLens => vl.append(lens)
        case a => a // actually, it's an error
      }
    case a => a
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
          Eval later opsToSingle(ops).wrap(ctx => (ctx.copy(peerId = valueToData(peerId)), _.copy(peerId = ctx.peerId)))
        case (NextTag(item), ops) =>
          Eval later new AirGen {

            override def generate(ctx: AirContext): (AirContext, Air) =
              ctx.data(item) match {
                case DataView.Variable(v) => ctx -> Air.Next(v)
                case _ => ctx -> Air.Null
              }
          }
        case (MatchMismatchTag(left, right, shouldMatch), ops) =>
          Eval later new AirGen {

            override def generate(ctx: AirContext): (AirContext, Air) = {
              val l = AirGen.resolve(ctx, valueToData(left))
              val r = AirGen.resolve(ctx, valueToData(right))
              val (resCtx, resAir) = opsToSingle(ops).generate(ctx)
              resCtx -> (if (shouldMatch) Air.Match(l, r, resAir) else Air.Mismatch(l, r, resAir))
            }
          }
        case (ForTag(item, iterable), ops) =>
          Eval later new AirGen {

            private val opWrap = opsToSingle(ops).wrap(ctx =>
              (if (ctx.vars(item)) {
                 val vn = item + ctx.instrCounter
                 ctx.copy(vars = ctx.vars + vn, data = ctx.data.updated(item, DataView.Variable(vn)))
               } else
                 ctx.copy(vars = ctx.vars + item, data = ctx.data.updated(item, DataView.Variable(item)))) -> (cu =>
                cu.copy(data = cu.data - item)
              )
            )

            override def generate(ctx: AirContext): (AirContext, Air) = {
              val varName =
                if (ctx.vars(item))
                  item + ctx.instrCounter
                else item

              val iterData = AirGen.resolve(ctx, valueToData(iterable))

              val (resCtx, resAir) = opWrap.generate(ctx)

              resCtx -> Air.Fold(iterData, varName, resAir)
            }
          }
        case (CoalgebraTag(ability, funcName, args, exportTo), _) =>
          Eval.later(ability match {
            case Some(ServiceModel(_, id)) =>
              new SrvCallable(valueToData(id), funcName).toCallGen(args.map(_._1).map(valueToData), exportTo)
            case None =>
              new AirGen {

                override def generate(ctx: AirContext): (AirContext, Air) =
                  ctx.arrows(funcName).toCallGen(args.map(_._1).map(valueToData), exportTo).generate(ctx)
              }
          })

      }
      .value
}

case object NullGen extends AirGen {
  override def generate(ctx: AirContext): (AirContext, Air) = (ctx, Air.Null)
}

case class SeqGen(left: AirGen, right: AirGen) extends AirGen {

  override def generate(ctx: AirContext): (AirContext, Air) = {
    val (c, l) = left.generate(ctx)
    right.generate(c).swap.map(_.incr).swap.map(Air.Seq(l, _))
  }
}

case class ServiceCallGen(
  srvId: DataView,
  fnName: String,
  args: List[DataView],
  result: Option[String]
) extends AirGen {

  override def generate(ctx: AirContext): (AirContext, Air) = {
    val (c, res) = result.fold(ctx -> Option.empty[String]) {
      case r if ctx.vars(r) =>
        val vn = r + ctx.instrCounter
        ctx.copy(vars = ctx.vars + vn, data = ctx.data.updated(r, DataView.Variable(vn))) -> Option(vn)
      case r =>
        ctx.copy(vars = ctx.vars + r, data = ctx.data.updated(r, DataView.Variable(r))) -> Option(r)
    }

    c.incr -> Air.Call(
      Triplet.Full(AirGen.resolve(ctx, ctx.peerId), AirGen.resolve(ctx, srvId), fnName),
      args.map(AirGen.resolve(ctx, _)),
      res
    )
  }
}

case class ParGen(left: AirGen, right: AirGen) extends AirGen {

  override def generate(ctx: AirContext): (AirContext, Air) = {
    val (lc, la) = left.generate(ctx)
    val (rc, ra) = right.generate(ctx.incr)
    (lc.mergePar(rc).incr, Air.Par(la, ra))
  }
}

case class XorGen(left: AirGen, right: AirGen) extends AirGen {

  override def generate(ctx: AirContext): (AirContext, Air) = {
    val (lc, la) = left.generate(ctx)
    val (rc, ra) = right.generate(ctx.incr)
    (lc.mergePar(rc).incr, Air.Xor(la, ra))
  }
}
