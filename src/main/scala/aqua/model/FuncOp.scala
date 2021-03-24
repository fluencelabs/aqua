package aqua.model

import aqua.semantics.Type
import cats.data.{NonEmptyChain, NonEmptyList}
import cats.kernel.Semigroup
import cats.syntax.semigroup._

sealed trait FuncOp extends Model

trait RightBiased {
  self: FuncOp =>
  def :+:(prev: FuncOp): FuncOp
}

object FuncOp {

  implicit object MergeOps extends Semigroup[FuncOp] {

    override def combine(x: FuncOp, y: FuncOp): FuncOp = (x, y) match {
      case (l: SeqModel, r: SeqModel) => SeqModel(l.ops ++ r.ops)
      case (l: SeqModel, r) => SeqModel(l.ops.append(r))
      case (l, r) => SeqModel(NonEmptyChain(l, r))
    }
  }
}

case class SeqModel(ops: NonEmptyChain[FuncOp]) extends FuncOp

case class ParModel(ops: NonEmptyList[FuncOp]) extends FuncOp with RightBiased {
  override def :+:(prev: FuncOp): FuncOp = copy(ops.prepend(prev))
}

case class XorModel(ops: NonEmptyList[FuncOp]) extends FuncOp with RightBiased {
  override def :+:(prev: FuncOp): FuncOp = copy(ops.prepend(prev))
}

case class OnModel(peerId: ValueModel, op: FuncOp) extends FuncOp

case class NextModel(item: String) extends FuncOp

case class MatchMismatchModel(left: ValueModel, right: ValueModel, shouldMatch: Boolean, op: FuncOp) extends FuncOp

case class ForModel(item: String, iterable: ValueModel, op: FuncOp) extends FuncOp {

  def body: FuncOp = op match {
    case ParModel(pars) => ParModel(pars.append(NextModel(item)))
    case _ => op |+| NextModel(item)
  }
}

case class CoalgebraModel(
  ability: Option[AbilityModel],
  funcName: String,
  args: List[(ValueModel, Type)],
  exportTo: Option[String]
) extends FuncOp
