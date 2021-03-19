package aqua.model

import aqua.generator.{AirContext, AirGen, DataView, ParGen, SeqGen, SrvCallable}
import aqua.semantics.Type
import cats.data.{NonEmptyChain, NonEmptyList}
import cats.kernel.Semigroup

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
