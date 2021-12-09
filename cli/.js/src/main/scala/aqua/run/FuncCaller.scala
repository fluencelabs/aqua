package aqua.run

import aqua.LogLevelTransformer
import aqua.backend.FunctionDef
import aqua.builder.{ConsoleServiceBuilder, FinisherBuilder}
import aqua.io.OutputPrinter
import aqua.js.{CallJsFunction, Fluence, FluenceUtils, PeerConfig}
import aqua.run.RunCommand.keyPairOrNull
import cats.effect.{Resource, Sync}
import cats.effect.kernel.Async
import cats.syntax.applicative.*

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js.JSON

object FuncCaller {

  /**
   * Register services and call an air code with FluenceJS SDK.
   * @param multiaddr relay to connect to
   * @param air code to call
   * @return
   */
  def funcCall[F[_]: Async](
    multiaddr: String,
    air: String,
    functionDef: FunctionDef,
    config: RunConfig,
    consoleService: ConsoleServiceBuilder,
    finisherService: FinisherBuilder
  )(implicit
    ec: ExecutionContext
  ): F[Unit] = {
    FluenceUtils.setLogLevel(LogLevelTransformer.logLevelToFluenceJS(config.logLevel))

    // stops peer in any way at the end of execution
    val resource = Resource.make(Fluence.getPeer().pure[F]) { peer =>
      Async[F].fromFuture(Sync[F].delay(peer.stop().toFuture))
    }

    resource.use { peer =>
      Async[F].fromFuture {
        (for {
          secretKey <- keyPairOrNull(config.secretKey)
          _ <- Fluence
            .start(
              PeerConfig(
                multiaddr,
                config.timeout,
                LogLevelTransformer.logLevelToAvm(config.logLevel),
                secretKey
              )
            )
            .toFuture
          _ = OutputPrinter.print("Your peerId: " + peer.getStatus().peerId)
          _ = consoleService.registerService(peer)
          _ = finisherService.registerService(peer)
          _ = config.argumentGetters.values.map(_.registerService(peer))
          callFuture = CallJsFunction.funcCallJs(
            air,
            functionDef,
            List.empty
          )
          _ <- Future.firstCompletedOf(finisherService.promise.future :: callFuture :: Nil)
        } yield {}).recover(handleFuncCallErrors).pure[F]
      }
    }
  }

  private def handleFuncCallErrors: PartialFunction[Throwable, Unit] = { t =>
    val message = if (t.getMessage.contains("Request timed out after")) {
      "Function execution failed by timeout. You can increase timeout with '--timeout' option in milliseconds or check if your code can hang while executing."
    } else JSON.stringify(t.toString)

    OutputPrinter.error(message)
  }
}
