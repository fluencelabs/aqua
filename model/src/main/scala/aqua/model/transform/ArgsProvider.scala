package aqua.model.transform

import aqua.model.ValueModel
import aqua.model.func.Call
import aqua.model.func.body.{FuncOp, FuncOps}

trait ArgsProvider {
  def transform(op: FuncOp): FuncOp
}

case class ArgsFromService(dataServiceId: ValueModel, names: Seq[String]) extends ArgsProvider {

  def getDataOp(name: String): FuncOp =
    FuncOps.callService(
      dataServiceId,
      name,
      Call(Nil, Some(name))
    )

  def transform(op: FuncOp): FuncOp =
    FuncOps.seq(
      names.map(getDataOp) :+ op: _*
    )

}
