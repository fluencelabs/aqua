package aqua.model

import aqua.semantics.{ArrowType, DataType, Type}
import cats.data.{Chain, NonEmptyChain}

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
      CoalgebraModel(
        Some(ServiceModel(callbackService, LiteralModel("\"" + callbackService + "\""))),
        respFuncName,
        (dv, t) :: Nil,
        None
      )
    )
  }

  // TODO rename
  def generateTsModel: FuncOp = SeqModel(
    NonEmptyChain
      .fromChainAppend(
        Chain.fromSeq(
          args.collect { case (argName, Left(_)) =>
            getDataOp(argName)
          } :+ getDataOp("relay")
        ),
        body
      )
      .appendChain(Chain.fromSeq(returnCallback.toSeq))
  )

  def getDataOp(name: String): FuncOp =
    CoalgebraModel(
      Some(ServiceModel(getDataService, LiteralModel("\"" + getDataService + "\""))),
      name,
      Nil,
      Some(name)
    )

  def viaRelay(op: FuncOp): FuncOp =
    OnModel(
      VarModel("relay"),
      SeqModel(
        NonEmptyChain(
          CoalgebraModel(
            Some(ServiceModel("op", LiteralModel("\"op\""))),
            "identity",
            Nil,
            None
          ),
          OnModel(InitPeerIdModel, op)
        )
      )
    )

}
