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
    // Actual name of the argument
    name: String,
    // Variable name to store the value of the argument
    varName: String,
    // Type of the argument
    t: DataType | StreamType
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
      ForTag
        .nonBlocking(item, VarRaw(iter, ArrayType(t.element)))
        .wrap(
          SeqTag.wrap(
            PushToStreamTag(VarRaw(item, t.element), Call.Export(varName, t)).leaf,
            NextTag(item).leaf
          )
        )
    )
  }

  private def getDataOp(name: String, varName: String, t: DataType): RawTag.Tree =
    CallArrowRawTag
      .service(
        dataServiceId,
        name,
        Call(Nil, Call.Export(varName, t) :: Nil)
      )
      .leaf

  def getDataOp(arg: ArgsProvider.Arg): RawTag.Tree =
    arg.t match {
      case st: StreamType =>
        getStreamDataOp(arg.name, arg.varName, st)
      case dt: DataType =>
        getDataOp(arg.name, arg.varName, dt)
    }

  override def provideArgs(args: List[ArgsProvider.Arg]): List[RawTag.Tree] =
    args.map(getDataOp)

}
