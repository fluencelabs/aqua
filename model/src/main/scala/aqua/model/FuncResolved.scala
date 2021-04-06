package aqua.model

import aqua.model.body.{Call, CallServiceTag, FuncOp, OnTag, SeqTag, Topology}
import aqua.types.{ArrowType, DataType}
import cats.data.Chain

case class FuncResolved(name: String, func: FuncCallable) {
  val getDataService: String = "getDataSrv"
  val callbackService: String = "callbackSrv"

  val respFuncName = "response"
  val relayVarName = "relay"

  val callbackSrvId: ValueModel = LiteralModel("\"" + callbackService + "\"")
  val dataSrvId: ValueModel = LiteralModel("\"" + getDataService + "\"")

  val returnCallback: Option[FuncOp] = func.ret.map { case (dv, t) =>
    viaRelay(
      FuncOp.leaf(
        CallServiceTag(
          callbackSrvId,
          respFuncName,
          Call(
            (dv, t) :: Nil,
            None
          )
        )
      )
    )
  }

  // TODO it's an overkill
  def initPeerCallable(name: String, arrowType: ArrowType): FuncCallable =
    FuncCallable(
      viaRelay(
        FuncOp.leaf(
          CallServiceTag(
            callbackSrvId,
            name,
            Call(
              arrowType.args.zipWithIndex.map { case (t, i) =>
                VarModel(s"arg$i") -> t
              },
              arrowType.res.map(_ => "init_call_res")
            )
          )
        )
      ),
      arrowType.args.zipWithIndex.map {
        case (t: DataType, i) => s"arg$i" -> Left(t)
        case (t: ArrowType, i) => s"arg$i" -> Right(t)
      },
      arrowType.res.map(VarModel("init_call_res") -> _),
      Map.empty
    )

  // TODO rename
  def generateTsModel: FuncOp =
    Topology resolve
      FuncOp
        .node(
          SeqTag,
          Chain
            .fromSeq(
              func.args.collect { case (argName, Left(_)) =>
                getDataOp(argName)
              } :+ getDataOp(relayVarName)
            )
            .append(
              func
                .apply(
                  generateTsCall,
                  func.args.collect { case (argName, Right(arrowType)) =>
                    argName -> initPeerCallable(argName, arrowType)
                  }.toMap,
                  func.args.collect { case (argName, Left(_)) =>
                    argName
                  }.foldLeft(Set(relayVarName))(_ + _)
                )
                .value
                ._1
            ) ++ Chain.fromSeq(returnCallback.toSeq)
        )

  // Like it is called from TS
  def generateTsCall: Call =
    Call(
      func.args.map { case (k, e) =>
        (VarModel(k), e.fold(identity, identity))
      },
      None
    )

  // Get data with this name from a local service
  def getDataOp(name: String): FuncOp =
    FuncOp.leaf(
      CallServiceTag(
        dataSrvId,
        name,
        Call(Nil, Some(name))
      )
    )

  // Get to init user through a relay
  def viaRelay(op: FuncOp): FuncOp =
    FuncOp.wrap(OnTag(InitPeerIdModel, VarModel(relayVarName) :: Nil), op)
}
