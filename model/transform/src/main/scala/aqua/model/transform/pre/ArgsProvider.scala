package aqua.model.transform.pre

import aqua.raw.ops.*
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{ArrayType, DataType, StreamType}

import cats.data.Chain

trait ArgsProvider {
  def provideArgs(args: List[ArgsProvider.Arg]): List[RawTag.Tree]
}

object ArgsProvider {

  final case class Arg(
    name: String,
    varName: String,
    t: DataType
  )
}

case class ArgsFromService(dataServiceId: ValueRaw) extends ArgsProvider {

  private def getStreamDataOp(name: String, varName: String, t: StreamType): RawTag.Tree = {
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
          PushToStreamTag(VarRaw(item, t.element), Call.Export(varName, t)).leaf,
          NextTag(item).leaf
        )
      )
    )
  }

  def getDataOp(arg: ArgsProvider.Arg): RawTag.Tree =
    arg.t match {
      case st: StreamType =>
        getStreamDataOp(arg.name, arg.varName, st)
      case _ =>
        CallArrowRawTag
          .service(
            dataServiceId,
            arg.name,
            Call(Nil, Call.Export(arg.varName, arg.t) :: Nil)
          )
          .leaf
    }

  override def provideArgs(args: List[ArgsProvider.Arg]): List[RawTag.Tree] =
    args.map(getDataOp)

}
