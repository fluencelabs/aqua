package aqua.model.transform

import aqua.model.func.body._
import aqua.model.func.{ArgDef, ArgsCall, ArgsDef, Call, FuncCallable}
import aqua.model.{LiteralModel, VarModel}
import aqua.types.ScalarType.string
import aqua.types.ArrowType
import cats.data.Chain
import cats.free.Cofree

object ForClient {
  // TODO not a string
  private val lastErrorArg = Call.Arg(LiteralModel("%last_error%"), string)

  // Get to init user through a relay
  def viaRelay(op: FuncOp)(implicit conf: BodyConfig): FuncOp =
    FuncOps.onVia(LiteralModel.initPeerId, Chain.one(VarModel(conf.relayVarName)), op)

  def wrapXor(op: FuncOp)(implicit conf: BodyConfig): FuncOp =
    if (conf.wrapWithXor)
      FuncOp.node(
        XorTag,
        Chain(
          op,
          viaRelay(
            FuncOps.callService(
              conf.errorHandlingCallback,
              conf.errorFuncName,
              Call(
                lastErrorArg :: Nil,
                None
              )
            )
          )
        )
      )
    else op

  def returnCallback(func: FuncCallable)(implicit conf: BodyConfig): Option[FuncOp] = func.ret.map {
    retArg =>
      viaRelay(
        FuncOps.callService(
          conf.callbackSrvId,
          conf.respFuncName,
          Call(
            retArg :: Nil,
            None
          )
        )
      )
  }

  def initPeerCallable(name: String, arrowType: ArrowType)(implicit
    conf: BodyConfig
  ): FuncCallable = {
    val (args, call, ret) = ArgsCall.arrowToArgsCallRet(arrowType)
    FuncCallable(
      s"init_peer_callable_$name",
      viaRelay(
        FuncOps.callService(
          conf.callbackSrvId,
          name,
          call
        )
      ),
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

    // Like it is called from TS
    def funcArgsCall: Call =
      Call(
        func.args.toCallArgs,
        None
      )

    val funcAround: FuncCallable = FuncCallable(
      "funcAround",
      wrapXor(
        viaRelay(
          FuncOp
            .node(
              SeqTag,
              (
                func.args.dataArgNames.map(getDataOp) :+ getDataOp(conf.relayVarName)
              )
                .append(
                  FuncOp.leaf(
                    CallArrowTag(
                      func.funcName,
                      funcArgsCall
                    )
                  )
                ) ++ Chain.fromSeq(returnCallback(func).toSeq)
            )
        )
      ),
      ArgsDef(ArgDef.Arrow(func.funcName, func.arrowType) :: Nil),
      None,
      func.args.arrowArgs.collect { case ArgDef.Arrow(argName, arrowType) =>
        argName -> initPeerCallable(argName, arrowType)
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
