package aqua.model.transform

import aqua.model.body._
import aqua.model.{FuncCallable, FuncResolved, InitPeerIdModel, VarModel}
import aqua.types.{ArrowType, DataType}
import cats.data.Chain
import cats.free.Cofree

object ForClient {

  def apply(func: FuncResolved, conf: BodyConfig): Cofree[Chain, OpTag] = {
    import conf._

    // Get to init user through a relay
    def viaRelay(op: FuncOp): FuncOp =
      FuncOp.wrap(OnTag(InitPeerIdModel, VarModel(relayVarName) :: Nil), op)

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

    val body =
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
                func.func
                  .apply(
                    funcArgsCall,
                    func.func.args.collect { case (argName, Right(arrowType)) =>
                      argName -> initPeerCallable(argName, arrowType)
                    }.toMap,
                    func.func.args.collect { case (argName, Left(_)) =>
                      argName
                    }.foldLeft(Set(relayVarName))(_ + _)
                  )
                  .value
                  ._1
              ) ++ Chain.fromSeq(returnCallback.toSeq)
          )
      ).tree

    Topology.resolve(body)
  }
}
