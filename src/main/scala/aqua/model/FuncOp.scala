package aqua.model

import aqua.generator.{Air, AirContext, AirGen, DataView, ParGen, SeqGen, SrvCallable}
import aqua.semantics.Type
import cats.data.{NonEmptyChain, NonEmptyList}
import cats.kernel.Semigroup
import cats.syntax.semigroup._

sealed trait FuncOp extends Model {
  def toAirGen: AirGen
}

object FuncOp {

  implicit object MergeOps extends Semigroup[FuncOp] {

    override def combine(x: FuncOp, y: FuncOp): FuncOp = (x, y) match {
      case (l: ParModel, r: ParModel) => ParModel(l.ops ++ r.ops.toList)
      case (l, r: ParModel) => ParModel(l :: r.ops)
      case (l: SeqModel, r: SeqModel) => SeqModel(l.ops ++ r.ops)
      case (l: SeqModel, r) => SeqModel(l.ops.append(r))
      case (l, r) => SeqModel(NonEmptyChain(l, r))
    }
  }
}

case class SeqModel(ops: NonEmptyChain[FuncOp]) extends FuncOp {
  override def toAirGen: AirGen = ops.map(_.toAirGen).reduceLeft(SeqGen)
}

case class ParModel(ops: NonEmptyList[FuncOp]) extends FuncOp {
  override def toAirGen: AirGen = ops.map(_.toAirGen).reduceLeft(ParGen)
}

case class OnModel(peerId: DataView, op: FuncOp) extends FuncOp {

  override def toAirGen: AirGen =
    op.toAirGen.wrap(ctx => (ctx.copy(peerId = peerId), _.copy(peerId = ctx.peerId)))
}

case class NextModel(item: String) extends FuncOp {

  override def toAirGen: AirGen = new AirGen {

    override def generate(ctx: AirContext): (AirContext, Air) =
      ctx.data(item) match {
        case DataView.Variable(v) => ctx -> Air.Next(v)
        case _ => ctx -> Air.Null
      }
  }
}

case class ForModel(item: String, iterable: DataView, op: FuncOp) extends FuncOp {

  private val opWrap = (op match {
    case ParModel(pars) => ParModel(pars.append(NextModel(item)))
    case _ => op |+| NextModel(item)
  }).toAirGen.wrap(ctx =>
    (if (ctx.vars(item)) {
       val vn = item + ctx.instrCounter
       ctx.copy(vars = ctx.vars + vn, data = ctx.data.updated(item, DataView.Variable(vn)))
     } else
       ctx.copy(vars = ctx.vars + item, data = ctx.data.updated(item, DataView.Variable(item)))) -> (cu =>
      cu.copy(data = cu.data - item)
    )
  )

  override def toAirGen: AirGen =
    new AirGen {

      override def generate(ctx: AirContext): (AirContext, Air) = {
        val varName =
          if (ctx.vars(item))
            item + ctx.instrCounter
          else item

        val iterData = AirGen.resolve(ctx, iterable)

        val (resCtx, resAir) = opWrap.generate(ctx)

        resCtx -> Air.Fold(iterData, varName, resAir)
      }
    }
}

case class CoalgebraModel(
  ability: Option[AbilityModel],
  funcName: String,
  args: List[(DataView, Type)],
  exportTo: Option[String]
) extends FuncOp {

  def toAirGen: AirGen =
    ability match {
      case Some(ServiceModel(_, id)) =>
        new SrvCallable(id, funcName).toCallGen(args.map(_._1), exportTo)
      case None =>
        (ctx: AirContext) => ctx.arrows(funcName).toCallGen(args.map(_._1), exportTo).generate(ctx)
    }

}
