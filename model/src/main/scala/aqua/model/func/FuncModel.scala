package aqua.model.func

import aqua.model.func.raw.FuncOp
import aqua.model.{Model, ValueModel}
import aqua.types.ArrowType

case class FuncModel(
  name: String,
  arrow: ArrowModel
) extends Model {

  def capture(
    arrows: Map[String, FuncCallable],
    constants: Map[String, ValueModel]
  ): FuncCallable =
    FuncCallable(name, arrow.body.fixXorPar, arrow.`type`, arrow.ret, arrows, constants)

}
