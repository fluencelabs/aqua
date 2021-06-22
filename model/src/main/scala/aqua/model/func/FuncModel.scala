package aqua.model.func

import aqua.model.func.raw.FuncOp
import aqua.model.{Model, ValueModel}
import aqua.types.Type

case class FuncModel(
  name: String,
  args: ArgsDef,
  ret: Option[(ValueModel, Type)],
  body: FuncOp
) extends Model {

  def capture(
    arrows: Map[String, FuncCallable],
    constants: Map[String, ValueModel]
  ): FuncCallable =
    FuncCallable(name, body.fixXorPar, args, ret, arrows, constants)

}
