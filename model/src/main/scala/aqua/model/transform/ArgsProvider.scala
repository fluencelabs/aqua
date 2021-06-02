package aqua.model.transform

import aqua.model.{ValueModel, VarModel}
import aqua.model.func.Call
import aqua.model.func.body.{FuncOp, FuncOps}
import aqua.types.{ArrayType, DataType, OptionType, StreamType, Type}
import cats.data.Chain

trait ArgsProvider {
  def transform(op: FuncOp): FuncOp
}

case class ArgsFromService(dataServiceId: ValueModel, names: List[(String, DataType)])
    extends ArgsProvider {

  private def getDataElOp(name: String, t: DataType, el: Type): FuncOp = {
    val iter = s"$name-iter"
    val item = s"$name-item"
    FuncOps.seq(
      FuncOps.callService(
        dataServiceId,
        name,
        Call(Nil, Some(Call.Export(iter, t)))
      ),
      FuncOps.fold(
        item,
        VarModel(iter, ArrayType(el), Chain.empty),
        FuncOps.seq(
          FuncOps.identity(VarModel(item, el), Call.Export(name, t)),
          FuncOps.next(item)
        )
      )
    )
  }

  def getDataOp(name: String, t: DataType): FuncOp =
    t match {
      case StreamType(el) =>
        getDataElOp(name, t, el)
      case OptionType(el) =>
        getDataElOp(name, StreamType(el), el)
      case _ =>
        FuncOps.callService(
          dataServiceId,
          name,
          Call(Nil, Some(Call.Export(name, t)))
        )
    }

  def transform(op: FuncOp): FuncOp =
    FuncOps.seq(
      names.map((getDataOp _).tupled) :+ op: _*
    )

}
