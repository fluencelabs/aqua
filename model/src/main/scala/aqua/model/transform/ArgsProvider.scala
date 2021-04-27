package aqua.model.transform

import aqua.model.ValueModel
import aqua.model.func.Call
import aqua.model.func.body.{FuncOp, FuncOps}
import aqua.types.DataType

trait ArgsProvider {
  def transform(op: FuncOp): FuncOp
}

case class ArgsFromService(dataServiceId: ValueModel, names: List[(String, DataType)])
    extends ArgsProvider {

  def getDataOp(name: String, t: DataType): FuncOp =
    FuncOps.callService(
      dataServiceId,
      name,
      Call(Nil, Some(Call.Export(name, t)))
    )

  def transform(op: FuncOp): FuncOp =
    FuncOps.seq(
      names.map((getDataOp _).tupled) :+ op: _*
    )

}
