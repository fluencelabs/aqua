package aqua.model

import aqua.semantics.{ArrowType, DataType, Type}
import cats.Eval

case class FuncModel(
  name: String,
  args: List[(String, Either[DataType, ArrowType])],
  ret: Option[(ValueModel, Type)],
  body: FuncOp
) extends Model {

  def captureArrows(arrows: Map[String, FuncCallable]): Eval[FuncCallable] =
    Eval.now(FuncCallable(body, args, ret, arrows))

}
