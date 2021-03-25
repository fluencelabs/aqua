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

  val returnCallback: Option[FuncOp] = ret.map { case (dv, t) =>
    viaRelay(
      FuncOp.leaf(
        CoalgebraTag(
          Some(ServiceModel(callbackService, LiteralModel("\"" + callbackService + "\""))),
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
        } :+ getDataOp("relay")
      )
      .append(body) ++ Chain.fromSeq(returnCallback.toSeq)
  )

  def getDataOp(name: String): FuncOp =
    FuncOp.leaf(
      CoalgebraTag(
        Some(ServiceModel(getDataService, LiteralModel("\"" + getDataService + "\""))),
        name,
        Nil,
        Some(name)
      )
    )

  def viaRelay(op: FuncOp): FuncOp =
    FuncOp.node(
      OnTag(VarModel("relay")),
      Chain(
        FuncOp.leaf(
          CoalgebraTag(
            Some(ServiceModel("op", LiteralModel("\"op\""))),
            "identity",
            Nil,
            None
          )
        ),
        FuncOp.wrap(OnTag(InitPeerIdModel), op)
      )
    )

}
