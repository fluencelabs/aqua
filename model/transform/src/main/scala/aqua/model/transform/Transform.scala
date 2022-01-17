package aqua.model.transform

import aqua.model.inline.ArrowInliner
import aqua.model.inline.state.InliningState
import aqua.model.{CallModel, FuncArrow, OpModel, VarModel}
import aqua.raw.value.VarRaw
import aqua.model.transform.funcop.*
import aqua.model.transform.pre.{
  ArgsFromService,
  ArgsProvider,
  FuncPreTransformer,
  InitPeerCallable,
  InitViaRelayCallable
}
import aqua.model.transform.res.{FuncRes, NoAir, ResolvedOp}
import aqua.model.transform.topology.Topology
import aqua.raw.ops.FuncOp
import aqua.types.ScalarType
import cats.Eval
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
  def funcRes(func: FuncArrow, conf: TransformConfig): Eval[FuncRes] = {
    val initCallable: InitPeerCallable = InitViaRelayCallable(
      Chain.fromOption(conf.relayVarName.map(_ -> ScalarType.string))
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

    // Transform the body of the function: wrap it with initCallable, provide function arguments via service calls
    val transform: FuncOp => FuncOp =
      initCallable.transform _ compose argsProvider.transform

    // Callback on the init peer id, either done via relay or not
    val callback = initCallable.service(conf.callbackSrvId)

    // preTransformer is applied before function is inlined
    val preTransformer = FuncPreTransformer(
      transform,
      callback,
      conf.respFuncName
    )
    // PreTransformation is done, function is inlined, we have an OpModel.Tree that is ready for topology resolution
    val preparedFunc = funcToModelTree(func, preTransformer).map(errorsCatcher.transform)

    // Resolve the topology, clear the resulting tree
    val resultingTree = preparedFunc.flatMap(tree => Topology.resolve(tree).map(clear(_)))

    resultingTree.map(res =>
      FuncRes(
        func,
        conf,
        res
      )
    )

  }

  def funcToModelTree(
    func: FuncArrow,
    preTransformer: FuncPreTransformer,
    funcArgName: String = "_func"
  ): Eval[OpModel.Tree] =
    ArrowInliner
      .callArrow[InliningState](
        preTransformer.preTransform(func),
        CallModel(VarModel(funcArgName, func.arrowType, Chain.empty) :: Nil, Nil)
      )
      .run(
        InliningState(resolvedArrows = Map(funcArgName -> func))
      )
      .map(_._2)
}
