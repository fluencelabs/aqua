package aqua.model

import cats.kernel.Semigroup

import scala.collection.immutable.Queue

trait Model

object Model {
  def empty(log: String): Model = EmptyModel(log)
  def error(log: String): Model = EmptyModel(log)

  implicit object MergeModels extends Semigroup[Model] {

    override def combine(x: Model, y: Model): Model = (x, y) match {
      case (l: FuncOp, r: FuncOp) =>
        FuncOp.MergeOps.combine(l, r)
      case (l: ScriptModel, r: ScriptModel) => ScriptModel(l.funcs ++ r.funcs)
      case (l: FuncModel, r: FuncModel) => ScriptModel(Queue(l, r))
      case (l: ScriptModel, r: FuncModel) => ScriptModel(l.funcs.appended(r))
      case (l: FuncModel, r: ScriptModel) => ScriptModel(r.funcs.prepended(l))
      case (_, r: ScriptModel) => r
      case (l: ScriptModel, _) => l
      case (_, r: FuncModel) => r
      case (l: FuncModel, _) => l
      case (l: EmptyModel, r: EmptyModel) => EmptyModel(l.log + " |+| " + r.log)
      case (_: EmptyModel, r) => r
      case (l, _: EmptyModel) => l
    }
  }
}

case class EmptyModel(log: String) extends Model
