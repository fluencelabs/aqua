package aqua.model

import aqua.model.func.raw.FuncOp
import cats.kernel.Semigroup

trait Model

object Model {
  def empty(log: String): Model = EmptyModel(log)
  def error(log: String): Model = EmptyModel(log)

  implicit object MergeModels extends Semigroup[Model] {

    override def combine(x: Model, y: Model): Model = (x, y) match {
      case (l: FuncOp, r: FuncOp) =>
        FuncOp.FuncOpSemigroup.combine(l, r)
      case (l: ScriptModel, r: ScriptModel) =>
        ScriptModel.SMMonoid.combine(l, r)

      case (l: EmptyModel, r: EmptyModel) => EmptyModel(l.log + " |+| " + r.log)
      case (_: EmptyModel, r) => r
      case (l, _: EmptyModel) => l

      case (l, r: ScriptModel) =>
        ScriptModel.toScriptPart(l).fold(r)(ScriptModel.SMMonoid.combine(_, r))
      case (l: ScriptModel, r) =>
        ScriptModel.toScriptPart(r).fold(l)(ScriptModel.SMMonoid.combine(l, _))

      case (l, r) =>
        ScriptModel
          .toScriptPart(l)
          .fold(r)(ls =>
            ScriptModel.toScriptPart(r).fold(l)(rs => ScriptModel.SMMonoid.combine(ls, rs))
          )

    }
  }
}

case class EmptyModel(log: String) extends Model
