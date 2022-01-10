package aqua.model.transform

import aqua.model.VarModel
import aqua.model.transform.funcop.*
import aqua.model.transform.res.{FuncRes, NoAir, ResolvedOp}
import aqua.model.transform.topology.Topology
import aqua.types.ScalarType
import cats.data.Chain
import cats.free.Cofree
import scribe.Logging

// TODO: doc
object Transform extends Logging {

  // TODO: doc
  def defaultFilter(t: ResolvedOp): Boolean = t match {
    case _: NoAir => false
    case _ => true
  }

  // TODO: doc
  def clear(
    tree: Cofree[Chain, ResolvedOp],
    filter: ResolvedOp => Boolean = defaultFilter
  ): Cofree[Chain, ResolvedOp] =
    tree.copy(tail = tree.tail.map(_.filter(t => filter(t.head)).map(clear(_, filter))))

  // TODO: doc/rename
  def fn(func: FuncCallable, conf: TransformConfig): FuncRes = {
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

    // TODO: comments
    val transform =
      initCallable.transform _ compose argsProvider.transform

    val callback = initCallable.service(conf.callbackSrvId)

    // TODO: comments/rename value
    val wrapFunc = ResolveFunc(
      transform,
      callback,
      conf.respFuncName
    )

    FuncRes(
      func,
      conf,
      clear(
        // TODO: comments
        Topology.resolve(
          errorsCatcher
            .transform(
              // TODO: comments
              wrapFunc.resolve(func).value
            )
            .tree
        )
      )
    )
  }
}
