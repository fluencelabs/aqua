package aqua.js
import aqua.backend.*
import aqua.definitions.{FunctionDef, ServiceDef}
import aqua.model.transform.TransformConfig
import aqua.res.FuncRes
import aqua.types.Type

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

trait ServiceHandler extends js.Function {
  def apply(args: js.Any*): js.Promise[js.Dynamic]
}

object CallJsFunction {

  def registerService(
    peer: FluencePeer,
    serviceId: String,
    handlers: List[(String, ServiceHandler)],
    servideDef: ServiceDef
  ): Unit = {
    val funcs = js.Dictionary.apply(handlers: _*)
    val args: js.Array[js.Any] =
      js.Array(peer, serviceId, funcs)
    V3.registerService(args, ServiceDefJs(servideDef))
  }

  // Call a function with generated air script
  def funcCallJs(
    air: String,
    functionDef: FunctionDef,
    args: List[js.Any]
  ): Future[Any] = {
    V3.callFunction(
      args.toJSArray,
      FunctionDefJs(functionDef),
      air
    ).toFuture
  }

}
