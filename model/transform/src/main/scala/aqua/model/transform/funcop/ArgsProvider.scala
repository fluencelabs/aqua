package aqua.model.transform.funcop

import aqua.model.{OpModel, ValueModel}
import aqua.raw.ops.{Call, FuncOp, FuncOps}
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{ArrayType, DataType, StreamType}
import cats.data.Chain

trait ArgsProvider {
  def transform(op: OpModel.Tree): OpModel.Tree
}

case class ArgsFromService(dataServiceId: ValueModel, names: List[(String, DataType)])
    extends ArgsProvider {

  private def getStreamDataOp(name: String, t: StreamType): OpModel.Tree = {
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
        VarRaw(iter, ArrayType(t.element)),
        FuncOps.seq(
          FuncOps.pushToStream(VarRaw(item, t.element), Call.Export(name, t)),
          FuncOps.next(item)
        )
      )
    )
  }

  def getDataOp(name: String, t: DataType): OpModel.Tree =
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
