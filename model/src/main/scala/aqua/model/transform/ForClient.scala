package aqua.model.transform

import aqua.model.func.body._
import aqua.model.func.{ArgDef, ArgsCall, ArgsDef, Call, FuncCallable}
import aqua.model.VarModel
import aqua.types.ArrowType
import cats.data.Chain
import cats.free.Cofree

object ForClient {
  type Service = (String, Call) => FuncOp

  def returnCallback(func: FuncCallable, callback: Service)(implicit
    conf: BodyConfig
  ): Option[FuncOp] = func.ret.map { retArg =>
    callback(
      conf.respFuncName,
      Call(
        retArg :: Nil,
        None
      )
    )
  }

  def initPeerCallable(name: String, arrowType: ArrowType, callback: Service): FuncCallable = {
    val (args, call, ret) = ArgsCall.arrowToArgsCallRet(arrowType)
    FuncCallable(
      s"init_peer_callable_$name",
      callback(name, call),
      args,
      ret,
      Map.empty
    )
  }

  // Get data with this name from a local service
  def getDataOp(name: String)(implicit conf: BodyConfig): FuncOp =
    FuncOps.callService(
      conf.dataSrvId,
      name,
      Call(Nil, Some(name))
    )

  def resolve(func: FuncCallable, conf: BodyConfig): Cofree[Chain, OpTag] = {
    implicit val c: BodyConfig = conf

    val initCallable: InitPeerCallable = InitViaRelayCallable(
      Chain.one(VarModel(conf.relayVarName))
    )
    val errorsCatcher = ErrorsCatcher(
      enabled = true,
      conf.errorHandlingCallback,
      conf.errorFuncName,
      initCallable
    )

    val tr = errorsCatcher.transform andThen initCallable.transform

    val callback = initCallable.service(conf.callbackSrvId)

    // Like it is called from TS
    def funcArgsCall: Call =
      Call(
        func.args.toCallArgs,
        None
      )

    val funcAround: FuncCallable = FuncCallable(
      "funcAround",
      tr(
        FuncOps.seq(
          getDataOp(conf.relayVarName),
          (func.args.dataArgNames.map(getDataOp) :+ FuncOps
            .callArrow(func.funcName, funcArgsCall)).toVector ++ returnCallback(func, callback): _*
        )
      ),
      ArgsDef(ArgDef.Arrow(func.funcName, func.arrowType) :: Nil),
      None,
      func.args.arrowArgs.collect { case ArgDef.Arrow(argName, arrowType) =>
        argName -> initPeerCallable(argName, arrowType, callback)
      }.toList.toMap
    )

    val body =
      funcAround
        .resolve(
          Call(Call.Arg(VarModel("_func"), func.arrowType) :: Nil, None),
          Map("_func" -> func),
          Set.empty
        )
        .value
        ._1
        .tree

    Topology.resolve(body)
  }
}
