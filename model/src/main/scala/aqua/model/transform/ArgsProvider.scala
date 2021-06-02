package aqua.model.transform

import aqua.model.func.Call
import aqua.model.func.body.{FuncOp, FuncOps}
import aqua.model.{IntoIndexModel, ValueModel, VarModel}
import aqua.types._
import cats.data.Chain

trait ArgsProvider {
  def transform(op: FuncOp): FuncOp
}

case class ArgsFromService(dataServiceId: ValueModel, names: List[(String, DataType)])
    extends ArgsProvider {

  private def getDataElOp(name: String, t: DataType, el: Type): FuncOp = {
    val iter = s"$name-iter"
    val item = s"$name-item"
    val acc = s"$name-acc"
    FuncOps.seq(
      FuncOps.callService(
        dataServiceId,
        name,
        Call(Nil, Some(Call.Export(iter, t)))
      ),
      FuncOps.seq(
        FuncOps.fold(
          item,
          VarModel(iter, ArrayType(el), Chain.empty),
          FuncOps.seq(
            FuncOps.identity(VarModel(item, el), Call.Export(acc, StreamType(t))),
            FuncOps.next(item)
          )
        ),
        FuncOps.identity(
          VarModel(acc, StreamType(t), Chain.one(IntoIndexModel(0, t))),
          Call.Export(name, t)
        )
      )
    )
  }

  def getDataOp(name: String, t: DataType): FuncOp =
    t match {
      case StreamType(el) =>
        getDataElOp(name, t, el)
      case OptionType(el) =>
        getDataElOp(name, ArrayType(el), el)
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
