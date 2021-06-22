package aqua.model.transform

import aqua.model.func.FuncCallable
import aqua.model.VarModel
import aqua.model.func.resolved.{NoAir, ResolvedOp}
import aqua.model.topology.Topology
import aqua.types.ScalarType
import cats.data.Chain
import cats.free.Cofree
import wvlet.log.LogSupport

object Transform extends LogSupport {

  def defaultFilter(t: ResolvedOp): Boolean = t match {
    case _: NoAir => false
    case _ => true
  }

  def clear(
    tree: Cofree[Chain, ResolvedOp],
    filter: ResolvedOp => Boolean = defaultFilter
  ): Cofree[Chain, ResolvedOp] =
    tree.copy(tail = tree.tail.map(_.filter(t => filter(t.head)).map(clear(_, filter))))

  def forClient(func: FuncCallable, conf: BodyConfig): Cofree[Chain, ResolvedOp] = {
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
        conf.relayVarName.map(_ -> ScalarType.string).toList ::: func.args.dataArgs.toList.map(
          add => add.name -> add.dataType
        )
      )

    val transform =
      initCallable.transform _ compose argsProvider.transform

    val callback = initCallable.service(conf.callbackSrvId)

    val wrapFunc = ResolveFunc(
      transform,
      callback,
      conf.respFuncName
    )
    clear(
      Topology.resolve(
        errorsCatcher
          .transform(
            wrapFunc.resolve(func).value
          )
          .tree
      )
    )
  }
}
