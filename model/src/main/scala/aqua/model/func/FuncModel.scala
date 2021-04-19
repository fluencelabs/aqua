package aqua.model.func

import aqua.model.func.body.FuncOp
import aqua.model.Model

case class FuncModel(
  name: String,
  args: ArgsDef,
  ret: Option[Call.Arg],
  body: FuncOp
) extends Model {

  def captureArrows(arrows: Map[String, FuncCallable]): FuncCallable =
    FuncCallable(body, args, ret, arrows)

}
