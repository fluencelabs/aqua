package aqua.model.transform.pre

import aqua.raw.ops.*
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{ArrayType, DataType, StreamType}
import cats.data.Chain

trait ArgsProvider extends PreTransform

case class ArgsFromService(dataServiceId: ValueRaw, names: List[(String, DataType)])
    extends ArgsProvider {

  private def getStreamDataOp(name: String, t: StreamType): FuncOp = {
    val iter = s"$name-iter"
    val item = s"$name-item"
    FuncOps.seq(
      FuncOp.leaf(
        CallServiceTag(
          dataServiceId,
          name,
          Call(Nil, Call.Export(iter, ArrayType(t.element)) :: Nil)
        )
      ),
      FuncOps.fold(
        item,
        VarRaw(iter, ArrayType(t.element)),
        FuncOps.seq(
          FuncOp.leaf(PushToStreamTag(VarRaw(item, t.element), Call.Export(name, t))),
          FuncOp.leaf(NextTag(item))
        )
      )
    )
  }

  def getDataOp(name: String, t: DataType): FuncOp =
    t match {
      case st: StreamType =>
        getStreamDataOp(name, st)
      case _ =>
        FuncOp.leaf(
          CallServiceTag(dataServiceId, name, Call(Nil, Call.Export(name, t) :: Nil))
        )
    }

  def transform(op: FuncOp): FuncOp =
    FuncOps.seq(
      names.map((getDataOp _).tupled) :+ op: _*
    )

}
