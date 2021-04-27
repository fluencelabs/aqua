package aqua.model.transform

import aqua.model.func.body._
import aqua.model.func.FuncCallable
import aqua.model.VarModel
import aqua.types.ScalarType
import cats.data.Chain
import cats.free.Cofree

object Transform {

  def forClient(func: FuncCallable, conf: BodyConfig): Cofree[Chain, OpTag] = {
    val initCallable: InitPeerCallable = InitViaRelayCallable(
      Chain.one(VarModel(conf.relayVarName, ScalarType.string))
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
        conf.relayVarName -> ScalarType.string :: func.args.dataArgs.toList.map(add =>
          add.name -> add.dataType
        )
      )

    val transform =
      errorsCatcher.transform _ compose initCallable.transform compose argsProvider.transform

    val callback = initCallable.service(conf.callbackSrvId)

    val wrapFunc = ResolveFunc(
      transform,
      callback,
      conf.respFuncName
    )

    Topology.resolve(wrapFunc.resolve(func).value.tree)
  }
}
