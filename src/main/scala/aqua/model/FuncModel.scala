package aqua.model

import aqua.semantics.{ArrowType, DataType, Type}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree

case class FuncModel(
  name: String,
  args: List[(String, Either[DataType, ArrowType])],
  ret: Option[(ValueModel, Type)],
  body: FuncOp
) extends Model {

  def toContext(arrows: Map[String, FuncCallable]): Eval[FuncCallable] =
    body
      .cata[Cofree[Chain, OpTag]] {
        case (CoalgebraTag(None, funcName, call), _) if arrows.contains(funcName) =>
          arrows(funcName).apply(call, arrows, Set.empty).map(_._1.tree)
        case (o, c) =>
          Eval.now(Cofree(o, Eval.now(c)))
      }
      .map(FuncOp(_))
      .map(FuncCallable(_, args, ret, arrows))

}
