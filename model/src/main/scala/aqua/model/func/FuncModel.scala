package aqua.model.func

import aqua.model.func.raw.FuncOp
import aqua.model.{Model, ValueModel}
import aqua.types.ArrowType

case class FuncModel(
  name: String,
  arrowType: ArrowType,
  ret: List[ValueModel],
  body: FuncOp
) extends Model {

  def capture(
    arrows: Map[String, FuncCallable],
    constants: Map[String, ValueModel]
  ): FuncCallable =
    FuncCallable(name, body.fixXorPar, arrowType, ret, arrows, constants)

}
