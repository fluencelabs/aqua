package aqua.model

import aqua.model.body.FuncOp
import aqua.types.{ArrowType, DataType, Type}

case class FuncModel(
  export: Boolean,
  name: String,
  args: List[(String, Either[DataType, ArrowType])],
  ret: Option[(ValueModel, Type)],
  body: FuncOp
) extends Model {

  def captureArrows(arrows: Map[String, FuncCallable]): FuncCallable =
    FuncCallable(body, args, ret, arrows)

}
