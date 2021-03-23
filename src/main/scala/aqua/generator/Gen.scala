package aqua.generator

import cats.syntax.functor._

sealed trait Gen

trait AirGen extends Gen {
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
