/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.model.transform

import aqua.model.*
import aqua.model.inline.ArrowInliner
import aqua.model.inline.state.{Config, InliningState}
import aqua.model.transform.TransformConfig.TracingConfig
import aqua.model.transform.funcop.*
import aqua.model.transform.pre.*
import aqua.model.transform.pre.{CallbackErrorHandler, ErrorHandler}
import aqua.model.transform.topology.Topology
import aqua.raw.ops.RawTag
import aqua.raw.value.VarRaw
import aqua.res.*
import aqua.types.ScalarType

import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import cats.instances.list.*
import cats.syntax.option.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import scribe.Logging

// API for transforming RawTag to Res
object Transform extends Logging {

  private def defaultFilter(t: ResolvedOp): Boolean = t match {
    case _: NoAir => false
    case _ => true
  }

  // Purge subtrees of tree for which `filter(head)` is false
  private def clear(
    tree: Cofree[Chain, ResolvedOp],
    filter: ResolvedOp => Boolean = defaultFilter
  ): Cofree[Chain, ResolvedOp] =
    Cofree.anaEval(tree)(
      tree =>
        for {
          children <- tree.tail
          filtered = children.filter(child => filter(child.head))
        } yield filtered,
      _.head
    )

  // Apply given preTransformer and inline the function
  private def funcToModelTree(
    func: FuncArrow,
    preTransformer: FuncPreTransformer,
    conf: TransformConfig,
    funcArgName: String = "_func"
  ): Eval[OpModel.Tree] = {

    /**
     * preTransform creates function
     * ```
     * func funcAround(func: <func.arrowType>):
     *   <retrieve args>
     *   result <- func(<args>)
     *   <pass result to callback>
     * ```
     */
    val transformed = preTransformer.preTransform(func)

    val funcArg = VarModel(funcArgName, func.arrowType)
    val call = CallModel(funcArg :: Nil, Nil)

    // <funcArgName> resolves to func
    val initState = InliningState(
      resolvedArrows = Map(funcArgName -> func),
      config = Config.Values(noErrorPropagation = conf.noXor)
    )

    // Inlining `funcAround(<funcArgName>)`
    ArrowInliner
      .callArrow[InliningState](transformed, call)
      .run(initState)
      .map { case (_, tree) => tree }
  }

  // Convert FuncArrow to FuncRes with given TransformConfig
  // Do necessary transformations and inlining
  def funcRes(func: FuncArrow, conf: TransformConfig): Eval[FuncRes] = {
    val relayVar = conf.relayVarName.map(_ -> ScalarType.string)

    val initCallable: InitPeerCallable = InitViaRelayCallable(
      goThrough = Chain.fromOption(relayVar)
    )

    val argsProvider: ArgsProvider = ArgsFromService(
      dataServiceId = conf.dataSrvId
    )

    val resultsHandler: ResultsHandler = CallbackResultsHandler(
      callbackSrvId = conf.callbackSrvId,
      funcName = conf.respFuncName,
      noEmptyResponse = conf.noEmptyResponse
    )

    val errorHandler: ErrorHandler = CallbackErrorHandler(
      serviceId = conf.errorHandlingSrvId,
      funcName = conf.errorFuncName
    )

    // Callback on the init peer id, either done via relay or not
    val callback = initCallable.service(conf.callbackSrvId)

    // preTransformer is applied before function is inlined
    val preTransformer = FuncPreTransformer(
      argsProvider,
      resultsHandler,
      errorHandler,
      callback,
      conf.relayVarName
    )

    val tracing = Tracing(
      enabledConfig = conf.tracing,
      initCallable = initCallable
    )

    for {
      // Pre transform and inline the function
      model <- funcToModelTree(func, preTransformer, conf)
      // Post transform the function.
      // We should wrap `model` with `onInitPeer` here
      // so that TagInliner would not wrap it with `xor`.
      // Topology module needs this `on`
      // as a starting point.
      initModel = initCallable.onInitPeer.wrap(model)
      tracingModel <- tracing(initModel)
      // Resolve topology
      resolved <- Topology.resolve(tracingModel)
      // Clear the tree
      result = clear(resolved)
    } yield FuncRes(
      func.funcName,
      func.argNames,
      FuncRes.arrowArgs(func.arrowType),
      func.arrowType.codomain,
      conf.relayVarName,
      conf.getDataService,
      conf.callbackService,
      conf.respFuncName,
      conf.errorHandlingService,
      conf.errorFuncName,
      result
    )
  }

  // Convert AquaContext to AquaRes with the given TransformConfig
  def contextRes(ex: AquaContext, conf: TransformConfig): AquaRes = {
    val funcResults = ex.funcs.toList.traverse { case (fnName, fn) =>
      funcRes(fn.copy(funcName = fnName), conf)
    }
    val serviceResults = ex.services.toList.map { case (srvName, srv) =>
      ServiceRes.fromModel(srv.copy(name = srvName))
    }

    AquaRes(
      funcs = Chain.fromSeq(funcResults.value),
      services = Chain.fromSeq(serviceResults)
    )
  }
}
