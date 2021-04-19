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

  def apply(func: FuncCallable, conf: BodyConfig): Cofree[Chain, OpTag] = {
    import conf._

    def wrapXor(op: FuncOp): FuncOp =
      if (wrapWithXor)
        FuncOp.node(
          XorTag,
          Chain(
            op,
            viaRelay(
              FuncOp.leaf(
                CallServiceTag(
                  errorHandlingCallback,
                  errorFuncName,
                  Call(
                    lastErrorArg :: Nil,
                    None
                  )
                )
              )
            )
          )
        )
      else op

    // Get to init user through a relay
    def viaRelay(op: FuncOp): FuncOp =
      FuncOp.wrap(OnTag(LiteralModel.initPeerId, Chain.one(VarModel(relayVarName))), op)

    val returnCallback: Option[FuncOp] = func.ret.map { retArg =>
      viaRelay(
        FuncOp.leaf(
          CallServiceTag(
            callbackSrvId,
            respFuncName,
            Call(
              retArg :: Nil,
              None
            )
          )
        )
      )
    }

    // TODO it's an overkill, is it?
    def initPeerCallable(name: String, arrowType: ArrowType): FuncCallable = {
      val (args, call, ret) = ArgsCall.arrowToArgsCallRet(arrowType)
      FuncCallable(
        s"init_peer_callable_$name",
        viaRelay(
          FuncOp.leaf(
            CallServiceTag(
              callbackSrvId,
              name,
              call
            )
          )
        ),
        args,
        ret,
        Map.empty
      )
    }

    // Like it is called from TS
    def funcArgsCall: Call =
      Call(
        func.args.toCallArgs,
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

    val funcAround: FuncCallable = FuncCallable(
      "funcAround",
      wrapXor(
        viaRelay(
          FuncOp
            .node(
              SeqTag,
              (
                func.args.dataArgNames.map(getDataOp) :+ getDataOp(relayVarName)
              )
                .append(
                  FuncOp.leaf(
                    CallArrowTag(
                      func.funcName,
                      funcArgsCall
                    )
                  )
                ) ++ Chain.fromSeq(returnCallback.toSeq)
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
        .apply(
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
