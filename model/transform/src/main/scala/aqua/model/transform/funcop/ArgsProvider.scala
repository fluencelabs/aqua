package aqua.model.transform.funcop

import aqua.model.{
  CallModel,
  CallServiceModel,
  OpModel,
  PushToStreamModel,
  SeqModel,
  ValueModel,
  VarModel
}
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
    SeqModel.wrap(
      CallServiceModel(
        dataServiceId,
        name,
        CallModel(Nil, CallModel.Export(iter, ArrayType(t.element)) :: Nil)
      ).leaf,
      FoldModel(item, VarModel(iter, ArrayType(t.element))).wrap(
        SeqModel.wrap(
          PushToStreamModel(VarModel(item, t.element), CallModel.Export(name, t)).leaf,
          NextModel(item).leaf
        )
      )
    )
  }

  def getDataOp(name: String, t: DataType): OpModel.Tree =
    t match {
      case st: StreamType =>
        getStreamDataOp(name, st)
      case _ =>
        CallServiceModel(dataServiceId, name, CallModel(Nil, CallModel.Export(name, t) :: Nil)).leaf
    }

  def transform(op: OpModel.Tree): OpModel.Tree =
    OpModel.seq(
      names.map((getDataOp _).tupled) :+ op: _*
    )

}
