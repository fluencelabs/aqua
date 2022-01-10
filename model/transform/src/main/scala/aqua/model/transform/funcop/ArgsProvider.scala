package aqua.model.transform.funcop

import aqua.model.{ValueModel, VarModel}
import aqua.model.func.raw.FuncOps
import aqua.raw.ops.{Call, FuncOp, FuncOps}
import aqua.types.{ArrayType, DataType, StreamType}
import cats.data.Chain

trait ArgsProvider {
  def transform(op: FuncOp): FuncOp
}

case class ArgsFromService(dataServiceId: ValueModel, names: List[(String, DataType)])
    extends ArgsProvider {

  private def getStreamDataOp(name: String, t: StreamType): FuncOp = {
    val iter = s"$name-iter"
    val item = s"$name-item"
    FuncOps.seq(
      FuncOps.callService(
        dataServiceId,
        name,
        Call(Nil, Call.Export(iter, ArrayType(t.element)) :: Nil)
      ),
      FuncOps.fold(
        item,
        VarModel(iter, ArrayType(t.element), Chain.empty),
        FuncOps.seq(
          FuncOps.pushToStream(VarModel(item, t.element), Call.Export(name, t)),
          FuncOps.next(item)
        )
      )
    )
  }

  def getDataOp(name: String, t: DataType): FuncOp =
    t match {
      case st: StreamType =>
        getStreamDataOp(name, st)
      case _ =>
        FuncOps.callService(
          dataServiceId,
          name,
          Call(Nil, Call.Export(name, t) :: Nil)
        )
    }

  def transform(op: FuncOp): FuncOp =
    FuncOps.seq(
      names.map((getDataOp _).tupled) :+ op: _*
    )

}
