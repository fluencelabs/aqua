package aqua.run

import aqua.LogLevelTransformer
import aqua.builder.{ArgumentGetter, Finisher, ResultPrinter, Service}
import aqua.definitions.FunctionDef
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

import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future, Promise, TimeoutException}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.{JSON, JavaScriptException, timers}

object FuncCaller {

  /**
   * Register services and call an air code with FluenceJS SDK.
   * @param air code to call
   * @return
   */
  def funcCall[F[_]: Async](
    name: String,
    air: String,
    functionDef: FunctionDef,
    config: RunConfig,
    finisherService: Finisher,
    services: List[Service],
    getters: List[ArgumentGetter],
    plugins: List[String]
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
            pc = PeerConfig(
              config.common.multiaddr,
              config.common.timeout.toMillis.toInt : js.UndefOr[Int],
              keyPair,
              Debug(printParticleId = config.common.flags.verbose, marineLogLevel = logLevel)
            )
            peerConfig = Some(
             pc.createObj()
            ).orUndefined
            _ <- Fluence.start(peerConfig).toFuture
            _ =
              if (config.common.flags.showConfig) {
                val configJson = KeyPairOp.toDynamicJSON(keyPair)
                configJson.updateDynamic("relay")(config.common.multiaddr)
                configJson.updateDynamic("timeout")(config.common.timeout.toMillis)
                configJson.updateDynamic("log-level")(config.common.logLevel.compiler.name)
                OutputPrinter.print(JSON.stringify(configJson, null, 4))
              }

            // register all services
            _ = (services ++ getters :+ finisherService).map(_.register(peer))
            // register all plugins
            plugins <- Plugin.getPlugins(plugins)
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
            finisher = setTimeout(name, finisherFuture, config.common.timeout)
            _ <- finisher
            _ <- Fluence.stop().toFuture
          } yield validNec(()))
            .recover(handleFuncCallErrors(name, config.common.timeout))
            .pure[F]
        }
      }

    }
  }

  private def setTimeout[T](funcName: String, f: Future[T], timeout: Duration)(implicit
    ec: ExecutionContext
  ): Future[T] = {
    val p = Promise[T]()
    val timeoutHandle =
      timers.setTimeout(timeout.toMillis)(
        p.tryFailure(new TimeoutException(timeoutErrorMessage(funcName, timeout, None)))
      )
    f.onComplete { result =>
      timers.clearTimeout(timeoutHandle)
      p.tryComplete(result)
    }
    p.future
  }

  private def timeoutErrorMessage(funcName: String, timeout: Duration, pid: Option[String]) = {
    val pidStr = pid.map(s => " " + s).getOrElse("")
    s"Function '$funcName' timed out after ${timeout.toMillis} milliseconds. Increase the timeout with '--timeout' option or check if your code can hang while executing$pidStr."
  }

  private def handleFuncCallErrors(
    funcName: String,
    timeout: Duration
  ): PartialFunction[Throwable, ValidatedNec[String, Unit]] = { t =>
    val message =
      t match {
        case te: TimeoutException => te.getMessage
        case t if t.getMessage.contains("Request timed out after") =>
          val msg = t.getMessage
          timeoutErrorMessage(
            funcName,
            timeout,
            Some(msg.substring(msg.indexOf("particle id") - 1, msg.length))
          )
        case tjs: JavaScriptException =>
          val msg = tjs.exception.asInstanceOf[js.Dynamic].selectDynamic("message")
          if (scalajs.js.isUndefined(msg)) JSON.stringify(tjs.exception.asInstanceOf[js.Any])
          else msg.toString
        case _ => t.toString
      }

    invalidNec(message)
  }
}
