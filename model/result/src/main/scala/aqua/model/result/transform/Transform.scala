package aqua.model.result.transform

import aqua.model.func.FuncCallable
import aqua.model.VarModel
import aqua.model.result.resolved.{NoAir, ResolvedOp}
import aqua.model.result.topology.Topology
import aqua.model.result.FuncRes
import aqua.types.ScalarType
import cats.data.Chain
import cats.free.Cofree
import scribe.Logging

object Transform extends Logging {

  def defaultFilter(t: ResolvedOp): Boolean = t match {
    case _: NoAir => false
    case _ => true
  }

  def clear(
    tree: Cofree[Chain, ResolvedOp],
    filter: ResolvedOp => Boolean = defaultFilter
  ): Cofree[Chain, ResolvedOp] =
    tree.copy(tail = tree.tail.map(_.filter(t => filter(t.head)).map(clear(_, filter))))

  def fn(func: FuncCallable, conf: GenerationConfig): FuncRes = {
    val initCallable: InitPeerCallable = InitViaRelayCallable(
      Chain.fromOption(conf.relayVarName).map(VarModel(_, ScalarType.string))
    )
    val errorsCatcher = ErrorsCatcher(
      enabled = conf.wrapWithXor,
      conf.errorHandlingCallback,
      conf.errorFuncName,
      initCallable
    )
    val argsProvider: ArgsProvider =
      ArgsFromService(
        conf.dataSrvId,
        conf.relayVarName.map(_ -> ScalarType.string).toList ::: func.arrowType.domain.labelledData
      )

    val transform =
      initCallable.transform _ compose argsProvider.transform

    val callback = initCallable.service(conf.callbackSrvId)

    val wrapFunc = ResolveFunc(
      transform,
      callback,
      conf.respFuncName
    )

    FuncRes(
      func,
      conf,
      clear(
        Topology.resolve(
          errorsCatcher
            .transform(
              wrapFunc.resolve(func).value
            )
            .tree
        )
      )
    )
  }
}
