package aqua.ast.gen

import cats.{Eval, Semigroup}
import cats.free.Free
import cats.syntax.functor._
import cats.syntax.show._

import scala.collection.immutable.Queue

sealed trait Gen {
  def lift[F[_]]: Free[F, Gen] = Free.pure(this)
}

object Gen {

  implicit object GenSemigroup extends Semigroup[Gen] {

    override def combine(x: Gen, y: Gen): Gen =
      (x, y) match {
        case (x: ScriptGen, y: ScriptGen) => y.copy(funcs = y.funcs.enqueueAll(x.funcs))
        case (x: FuncGen, y: FuncGen) => ScriptGen(Queue(x, y))
        case (x: FuncGen, y: ScriptGen) => y.copy(funcs = y.funcs.enqueue(x))
        case (x: AirGen, y: FuncBodyGen) => y.copy(op = SeqGen(x, y.op))
        case (x: AirGen, y: ParGen) => ParGen(Some(x), y.right)
        case (x: AirGen, y: AirGen) => SeqGen(x, y)

        case (NoopGen, _) => y
        case (_, NoopGen) => x

        case (_, y) =>
          println(Console.RED + s"drop x: ${x} in favor of y: $y" + Console.RESET)
          y
      }

  }

  def noop: Gen = NoopGen
  def error: Gen = NoopGen
}

sealed trait AirGen extends Gen {
  self =>
  def generate(ctx: AirContext): (AirContext, Air)

  def wrap(f: AirContext => (AirContext, AirContext => AirContext)): AirGen =
    new AirGen {

      override def generate(ctx: AirContext): (AirContext, Air) = {
        val (setup, clean) = f(ctx)
        val (internal, res) = self.generate(setup)
        (clean(internal), res)
      }
    }
}

case object NullGen extends AirGen {
  override def generate(ctx: AirContext): (AirContext, Air) = (ctx, Air.Null)
}

case object NoopGen extends Gen

case class SeqGen(left: AirGen, right: AirGen) extends AirGen {

  override def generate(ctx: AirContext): (AirContext, Air) = {
    println(Console.BLUE + ctx + Console.RESET)
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
      Triplet.Full(ctx.peerId, srvId, fnName),
      args.map {
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
      },
      res
    )
  }
}

case class FuncBodyGen(op: AirGen) extends Gen

case class FuncGen(name: String, air: Eval[Air], body: FuncBodyGen) extends Gen {
  def generateAir: Air = air.memoize.value
}

case class ScriptGen(funcs: Queue[FuncGen]) extends Gen {

  def generateAir: Queue[String] =
    funcs.map(_.generateAir.show)
}

case class ParGen(left: Option[AirGen], right: AirGen) extends AirGen {

  override def generate(ctx: AirContext): (AirContext, Air) =
    left.fold(right.generate(ctx)) { l =>
      val (lc, la) = l.generate(ctx)
      val (rc, ra) = right.generate(ctx.incr)
      (lc.mergePar(rc).incr, Air.Par(la, ra))
    }
}
