package aqua.model.transform

import aqua.model.body._
import aqua.model.{FuncCallable, FuncResolved, InitPeerIdModel, LiteralModel, VarModel}
import aqua.types.ScalarType.string
import aqua.types.{ArrowType, DataType}
import cats.data.Chain
import cats.free.Cofree

object ForClient {

  def apply(func: FuncResolved, conf: BodyConfig): Cofree[Chain, OpTag] = {
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
                    // TODO not a string
                    (LiteralModel("%last_error%"), string) :: Nil,
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
      FuncOp.wrap(OnTag(InitPeerIdModel, Chain.one(VarModel(relayVarName))), op)

    val returnCallback: Option[FuncOp] = func.func.ret.map { case (dv, t) =>
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

    // TODO it's an overkill, is it?
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

    // Like it is called from TS
    def funcArgsCall: Call =
      Call(
        func.func.args.map { case (k, e) =>
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

    val funcAround: FuncCallable = FuncCallable(
      wrapXor(
        viaRelay(
          FuncOp
            .node(
              SeqTag,
              Chain
                .fromSeq(
                  func.func.args.collect { case (argName, Left(_)) =>
                    getDataOp(argName)
                  } :+ getDataOp(relayVarName)
                )
                .append(
                  FuncOp.leaf(
                    CallArrowTag(
                      func.name,
                      funcArgsCall
                    )
                  )
                ) ++ Chain.fromSeq(returnCallback.toSeq)
            )
        )
      ),
      (func.name -> Right(func.arrowType)) :: Nil,
      None,
      func.func.args.collect { case (argName, Right(arrowType)) =>
        argName -> initPeerCallable(argName, arrowType)
      }.toMap
    )

    val body =
      funcAround
        .apply(
          Call((VarModel("_func") -> func.arrowType) :: Nil, None),
          Map("_func" -> func.func),
          Set.empty
        )
        .value
        ._1
        .tree

    Topology.resolve(body)
  }
}
