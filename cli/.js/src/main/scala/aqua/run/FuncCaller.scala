package aqua.run

import aqua.LogLevelTransformer
import aqua.backend.FunctionDef
import aqua.builder.{ArgumentGetter, Finisher, ResultPrinter, Service}
import aqua.io.OutputPrinter
import aqua.js.*
import aqua.keypair.KeyPairShow.show
import aqua.run.RunCommand.createKeyPair
import aqua.run.plugin.Plugin
import cats.data.Validated.{invalidNec, validNec}
import cats.data.ValidatedNec
import cats.effect.kernel.Async
import cats.effect.{Resource, Sync}
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.show.*

import scala.concurrent.{ExecutionContext, Future, Promise, TimeoutException}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.{timers, JSON, JavaScriptException}

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
    services: List[Service],
    getters: List[ArgumentGetter]
  ): F[ValidatedNec[String, Unit]] = {

    FluenceUtils.setLogLevel(
      LogLevelTransformer.logLevelToFluenceJS(config.common.logLevel.fluencejs)
    )

    // stops peer in any way at the end of execution
    val resource = Resource.make(Fluence.getPeer().pure[F]) { peer =>
      Async[F].fromFuture(Sync[F].delay(peer.stop().toFuture))
    }

    resource.use { peer =>
      Async[F].executionContext.flatMap { implicit ec =>
        Async[F].fromFuture {
          (for {
            keyPair <- createKeyPair(config.common.secretKey)
            logLevel: js.UndefOr[aqua.js.LogLevel] = LogLevelTransformer.logLevelToAvm(
              config.common.logLevel.aquavm
            )
            _ <- Fluence
              .start(
                Some(
                  PeerConfig(
                    config.common.multiaddr,
                    config.common.timeout.getOrElse(scalajs.js.undefined),
                    keyPair,
                    Debug(printParticleId = config.common.flags.verbose, marineLogLevel = logLevel)
                  )
                ).orUndefined
              )
              .toFuture
            _ =
              if (config.common.flags.showConfig) {
                val configJson = KeyPairOp.toDynamicJSON(keyPair)
                configJson.updateDynamic("relay")(config.common.multiaddr)
                config.common.timeout.foreach(t => configJson.updateDynamic("timeout")(t))
                configJson.updateDynamic("log-level")(config.common.logLevel.compiler.name)
                OutputPrinter.print(JSON.stringify(configJson, null, 4))
              }

            // register all services
            _ = (services ++ getters :+ finisherService).map(_.register(peer))
            // register all plugins
            plugins <- Plugin.getPlugins(config.plugins)
            _ = plugins.map(_.register(peer))
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
            _ <- Fluence.stop().toFuture
          } yield validNec(())).recover(handleFuncCallErrors).pure[F]
        }
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
    "Function execution failed by timeout. You can increase the timeout with '--timeout' option in milliseconds or check if your code can hang while executing."

  private def handleFuncCallErrors: PartialFunction[Throwable, ValidatedNec[String, Unit]] = { t =>
    val message =
      t match {
        case te: TimeoutException => te.getMessage
        case tjs: JavaScriptException =>
          val msg = tjs.exception.asInstanceOf[js.Dynamic].selectDynamic("message")
          if (scalajs.js.isUndefined(msg)) JSON.stringify(tjs.exception.asInstanceOf[js.Any])
          else msg.toString
        case _ =>
          if (t.getMessage.contains("Request timed out after")) {
            TimeoutErrorMessage
          } else JSON.stringify(t.toString)
      }

    invalidNec(message)
  }
}
