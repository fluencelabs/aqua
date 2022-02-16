package aqua.run

import aqua.LogLevelTransformer
import aqua.backend.FunctionDef
import aqua.builder.{Finisher, ResultPrinter, Service}
import aqua.io.OutputPrinter
import aqua.js.{CallJsFunction, Fluence, FluenceUtils, PeerConfig}
import aqua.run.RunCommand.createKeyPair
import cats.effect.{Resource, Sync}
import cats.effect.kernel.Async
import cats.syntax.applicative.*

import scala.concurrent.{ExecutionContext, Future, Promise, TimeoutException}
import scala.scalajs.js.JSON
import scala.scalajs.js.timers

object FuncCaller {

  /**
   * Register services and call an air code with FluenceJS SDK.
   * @param air code to call
   * @return
   */
  def funcCall[F[_]: Async](
    air: String,
    functionDef: FunctionDef,
    config: RunConfig,
    finisherService: Finisher,
    services: List[Service]
  )(implicit
    ec: ExecutionContext
  ): F[Unit] = {
    FluenceUtils.setLogLevel(LogLevelTransformer.logLevelToFluenceJS(config.common.logLevel))

    // stops peer in any way at the end of execution
    val resource = Resource.make(Fluence.getPeer().pure[F]) { peer =>
      Async[F].fromFuture(Sync[F].delay(peer.stop().toFuture))
    }

    resource.use { peer =>
      Async[F].fromFuture {
        (for {
          keyPair <- createKeyPair(config.common.secretKey)
          _ <- Fluence
            .start(
              PeerConfig(
                config.common.multiaddr,
                config.common.timeout.getOrElse(scalajs.js.undefined),
                LogLevelTransformer.logLevelToAvm(config.common.logLevel),
                keyPair.orNull
              )
            )
            .toFuture
          _ = OutputPrinter.print("Your peerId: " + peer.getStatus().peerId)
          // register all services
          _ = (services ++ config.argumentGetters.values :+ finisherService).map(_.register(peer))
          callFuture = CallJsFunction.funcCallJs(
            air,
            functionDef,
            List.empty
          )
          // error will be thrown on failed call
          _ <- callFuture
          finisherFuture = finisherService.promise.future
          // use a timeout in finisher if we have an async function and it hangs on node's side
          finisher = config.common.timeout.map { t =>
            setTimeout(finisherFuture, t)
          }.getOrElse(finisherFuture)
          _ <- finisher
        } yield ()).recover(handleFuncCallErrors).pure[F]
      }
    }
  }

  private def setTimeout[T](f: Future[T], timeout: Int)(implicit
    ec: ExecutionContext
  ): Future[T] = {
    val p = Promise[T]()
    val timeoutHandle =
      timers.setTimeout(timeout)(p.tryFailure(new TimeoutException(TimeoutErrorMessage)))
    f.onComplete { result =>
      timers.clearTimeout(timeoutHandle)
      p.tryComplete(result)
    }
    p.future
  }

  val TimeoutErrorMessage =
    "Function execution failed by timeout. You can increase timeout with '--timeout' option in milliseconds or check if your code can hang while executing."

  private def handleFuncCallErrors: PartialFunction[Throwable, Unit] = { t =>
    val message =
      t match {
        case te: TimeoutException => te.getMessage
        case _ =>
          if (t.getMessage.contains("Request timed out after")) {
            TimeoutErrorMessage
          } else JSON.stringify(t.toString)
      }

    OutputPrinter.error(message)
  }
}
