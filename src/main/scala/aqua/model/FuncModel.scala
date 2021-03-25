package aqua.model

import aqua.semantics.{ArrowType, DataType, Type}
import cats.data.Chain

case class FuncModel(
  name: String,
  args: List[(String, Either[DataType, ArrowType])],
  ret: Option[(ValueModel, Type)],
  body: FuncOp
) extends Model {

  val getDataService: String = "getDataSrv"
  val callbackService: String = "callbackSrv"

  val respFuncName = "response"
  val relayVarName = "relay"

  val returnCallback: Option[FuncOp] = ret.map { case (dv, t) =>
    viaRelay(
      FuncOp.leaf(
        CallServiceTag(
          LiteralModel("\"" + callbackService + "\""),
          respFuncName,
          (dv, t) :: Nil,
          None
        )
      )
    )
  }

  // TODO rename
  def generateTsModel: FuncOp = FuncOp.node(
    SeqTag,
    Chain
      .fromSeq(
        args.collect { case (argName, Left(_)) =>
          getDataOp(argName)
        } :+ getDataOp(relayVarName)
      )
      .append(body) ++ Chain.fromSeq(returnCallback.toSeq)
  )

  def getDataOp(name: String): FuncOp =
    FuncOp.leaf(
      CallServiceTag(
        LiteralModel("\"" + getDataService + "\""),
        name,
        Nil,
        Some(name)
      )
    )

  def viaRelay(op: FuncOp): FuncOp =
    FuncOp.node(
      OnTag(VarModel(relayVarName)),
      Chain(
        FuncOp.leaf(
          CallServiceTag(
            LiteralModel("\"op\""),
            "identity",
            Nil,
            None
          )
        ),
        FuncOp.wrap(OnTag(InitPeerIdModel), op)
      )
    )

}
