package aqua.model.func

import aqua.model.func.body.FuncOp
import aqua.model.{Model, ValueModel}

case class FuncModel(
  name: String,
  args: ArgsDef,
  ret: Option[Call.Arg],
  body: FuncOp
) extends Model {

  def captureParts(
    arrows: Map[String, FuncCallable],
    constants: Map[String, ValueModel]
  ): FuncCallable =
    FuncCallable(name, body, args, ret, arrows, constants)

}
