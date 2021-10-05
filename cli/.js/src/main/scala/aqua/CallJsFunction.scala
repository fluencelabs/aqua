package aqua

import aqua.model.transform.res.FuncRes

import scala.concurrent.{Future, Promise}
import scala.scalajs.js

object CallJsFunction {

  // Register a service that returns no result
  def registerUnitService(
    peer: FluencePeer,
    serviceId: String,
    fnName: String,
    handler: (js.Array[js.Any]) => Unit
  ) = {
    peer.internals.callServiceHandler.use((req, resp, next) => {
      if (req.serviceId == serviceId && req.fnName == fnName) {
        handler(req.args)
        resp.retCode = ResultCodes.success
        resp.result = new js.Object {}
      }

      next()
    })
  }

  // Call a function with generated air script
  def funcCallJs(
    peer: FluencePeer,
    fnName: String,
    air: String,
    args: List[(String, js.Any)],
    funcRes: FuncRes
  ): Future[Any] = {
    val resultPromise: Promise[js.Any] = Promise[js.Any]()

    val requestBuilder = new RequestFlowBuilder()
    val relayPeerId = peer.getStatus().relayPeerId

    requestBuilder
      .disableInjections()
      .withRawScript(air)
      .configHandler((handler, r) => {
        handler.on("getDataSrv", "-relay-", (_, _) => { relayPeerId })
        args.foreach { (fnName, arg) =>
          handler.on("getDataSrv", fnName, (_, _) => arg)
        }
        handler.onEvent(
          "callbackSrv",
          "response",
          (args, _) => {
            if (args.length == 1) {
              resultPromise.success(args.pop())
            } else if (args.length == 0) {
              resultPromise.success({})
            } else {
              resultPromise.success(args)
            }
            {}
          }
        )
        handler.onEvent(
          "errorHandlingSrv",
          "error",
          (args, _) => {
            resultPromise.failure(new RuntimeException(args.pop().toString))
            {}
          }
        )
      })
      .handleScriptError((err) => {
        resultPromise.failure(new RuntimeException("script error: " + err.toString))
      })
      .handleTimeout(() => {
        if (!resultPromise.isCompleted)
          resultPromise.failure(new RuntimeException(s"Request timed out for $fnName"))
      })

    peer.internals.initiateFlow(requestBuilder.build())

    funcRes.returnType.fold(Future.unit)(_ => resultPromise.future)
  }
}
