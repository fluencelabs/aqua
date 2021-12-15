package aqua.js

import aqua.backend.{ArgDefinition, FunctionDef, NamesConfig, TypeDefinition}
import aqua.model.transform.TransformConfig
import aqua.model.transform.res.FuncRes
import aqua.types.Type

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

object CallJsFunction {

  def registerService(
    peer: FluencePeer,
    serviceId: String,
    fnName: String,
    handler: js.Array[js.Any] => js.Dynamic
  ): CallServiceHandler = {
    peer.internals.callServiceHandler.use((req, resp, next) => {
      if (req.serviceId == serviceId && req.fnName == fnName) {
        val result = handler(req.args)
        resp.retCode = ResultCodes.success
        resp.result = result
      }

      next()
    })
  }

  // Call a function with generated air script
  def funcCallJs(
    air: String,
    functionDef: FunctionDef,
    args: List[js.Any]
  )(implicit ec: ExecutionContext): Future[Any] = {
    V2.callFunction(
      args.toJSArray,
      FunctionDefJs(functionDef),
      air
    ).toFuture
  }

}
