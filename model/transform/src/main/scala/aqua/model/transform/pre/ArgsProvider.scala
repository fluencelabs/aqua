package aqua.model.transform.pre

import aqua.raw.ops.*
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{ArrayType, DataType, StreamType}
import cats.data.Chain

trait ArgsProvider {
  def provideArgs(args: List[(String, DataType)]): List[RawTag.Tree]
}

case class ArgsFromService(dataServiceId: ValueRaw) extends ArgsProvider {

  private def getStreamDataOp(name: String, t: StreamType): RawTag.Tree = {
    val iter = s"$name-iter"
    val item = s"$name-item"
    SeqTag.wrap(
      CallArrowRawTag
        .service(
          dataServiceId,
          name,
          Call(Nil, Call.Export(iter, ArrayType(t.element)) :: Nil)
        )
        .leaf,
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
        CallArrowRawTag
          .service(
            dataServiceId,
            name,
            Call(Nil, Call.Export(name, t) :: Nil)
          )
          .leaf
    }

  override def provideArgs(args: List[(String, DataType)]): List[RawTag.Tree] =
    args.map(getDataOp.tupled)

}
