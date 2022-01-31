package aqua.model.transform.pre

import aqua.raw.ops.*
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{ArrayType, DataType, StreamType}
import cats.data.Chain

trait ArgsProvider extends PreTransform

case class ArgsFromService(dataServiceId: ValueRaw, names: List[(String, DataType)])
    extends ArgsProvider {

  private def getStreamDataOp(name: String, t: StreamType): RawTag.Tree = {
    val iter = s"$name-iter"
    val item = s"$name-item"
    SeqTag.wrap(
      CallServiceTag(
        dataServiceId,
        name,
        Call(Nil, Call.Export(iter, ArrayType(t.element)) :: Nil)
      ).leaf,
      ForTag(item, VarRaw(iter, ArrayType(t.element))).wrap(
        SeqTag.wrap(
          PushToStreamTag(VarRaw(item, t.element), Call.Export(name, t)).leaf,
          NextTag(item).leaf
        )
      )
    )
  }

  def getDataOp(name: String, t: DataType): RawTag.Tree =
    t match {
      case st: StreamType =>
        getStreamDataOp(name, st)
      case _ =>
        CallServiceTag(dataServiceId, name, Call(Nil, Call.Export(name, t) :: Nil)).leaf
    }

  def transform(op: RawTag.Tree): RawTag.Tree =
    SeqTag.wrap(
      names.map((getDataOp _).tupled) :+ op: _*
    )

}
