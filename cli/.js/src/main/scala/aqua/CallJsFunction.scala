package aqua

import aqua.model.transform.TransformConfig
import aqua.model.transform.res.FuncRes
import aqua.types.Type

import scala.concurrent.{ExecutionContext, Future, Promise}
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
    air: String,
    args: List[(String, js.Any)],
    returnType: Option[Type],
    config: TransformConfig
  )(implicit ec: ExecutionContext): Future[Any] = {
    val resultPromise: Promise[js.Any] = Promise[js.Any]()

    val requestBuilder = new RequestFlowBuilder()
    val relayPeerId = peer.getStatus().relayPeerId

    requestBuilder
      .disableInjections()
      .withRawScript(air)
      .configHandler((handler, r) => {
        handler.on(config.getDataService, config.relayVarName.getOrElse("-relay-"), (_, _) => { relayPeerId })
        args.foreach { (fnName, arg) =>
          handler.on(config.getDataService, fnName, (_, _) => arg)
        }
        handler.onEvent(
          config.callbackService,
          config.respFuncName,
          (args, _) => {
            if (args.length == 1) {
              resultPromise.success(args.pop())
            } else if (args.length == 0) {
              resultPromise.success(())
            } else {
              resultPromise.success(args)
            }
            ()
          }
        )
        handler.onEvent(
          config.errorHandlingService,
          config.errorFuncName,
          (args, _) => {
            resultPromise.failure(new RuntimeException(args.pop().toString))
            ()
          }
        )
      })
      .handleScriptError((err) => {
        resultPromise.failure(new RuntimeException("script error: " + err.toString))
      })
      .handleTimeout(() => {
        if (!resultPromise.isCompleted)
          resultPromise.failure(new RuntimeException(s"Request timed out"))
      })

    peer.internals.initiateFlow(requestBuilder.build()).toFuture.flatMap { _ =>
      returnType.fold(resultPromise.success(()).future)(_ => resultPromise.future)
    }
  }
}
