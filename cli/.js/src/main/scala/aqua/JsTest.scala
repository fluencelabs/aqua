package aqua

import aqua.backend.air.AirBackend
import aqua.backend.js.JavaScriptBackend
import aqua.backend.ts.TypeScriptBackend
import aqua.compiler.AquaCompiler
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.io.AquaFileError
import aqua.model.transform.TransformConfig
import aqua.parser.lift.FileSpan
import cats.data.Validated
import cats.effect.{IO, IOApp, Sync}
import fs2.io.file.Path
import cats.data.Chain

import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.annotation.*

trait Internals extends js.Object {
//  def initiateFlow(r: RequestFlow) = js.native
  def initiateFlow(r: RequestFlow): Promise[js.Any]
}

trait Status extends js.Object {
  //  def initiateFlow(r: RequestFlow) = js.native
  def relayPeerId: String
}

@js.native
@JSImport("@fluencelabs/fluence/dist/internal/compilerSupport/v1.js", "FluencePeer")
class FluencePeer extends js.Object {
  val internals: Internals = js.native
  def getStatus(): Status = js.native
}

@js.native
@JSImport("@fluencelabs/fluence", "Fluence")
object Fluence extends js.Object {
  def start(str: String): js.Promise[js.Any] = js.native
  def getPeer(): FluencePeer = js.native
}

@js.native
@JSImport("@fluencelabs/fluence/dist/internal/compilerSupport/v1.js", "CallServiceHandler")
class CallServiceHandler extends js.Object {

  def on(
    serviceId: String,
    fnName: String,
    handler: js.Function2[js.Array[js.Any], js.Any, js.Any]
  ): js.Function0[CallServiceHandler] = js.native

  def onEvent(
    serviceId: String,
    fnName: String,
    handler: js.Function2[js.Array[js.Any], js.Any, js.Any]
  ): js.Function0[CallServiceHandler] = js.native
}

@js.native
@JSImport("@fluencelabs/fluence/dist/internal/compilerSupport/v1.js", "RequestFlow")
class RequestFlow extends js.Object {}

@js.native
@JSImport("@fluencelabs/fluence/dist/internal/compilerSupport/v1.js", "RequestFlowBuilder")
class RequestFlowBuilder extends js.Object {
  def withRawScript(air: String): RequestFlowBuilder = js.native

  def configHandler(f: js.Function2[CallServiceHandler, js.Any, Unit]): RequestFlowBuilder =
    js.native
  def disableInjections(): RequestFlowBuilder = js.native
  def build(): RequestFlow = js.native
  def handleScriptError(f: js.Function1[js.Any, Unit]): RequestFlowBuilder = js.native
  def handleTimeout(f: js.Function0[Unit]): RequestFlowBuilder = js.native

}

//@js.native
//@JSImport("./compiled/caller.js", JSImport.Namespace)
//object Caller extends js.Object {
//  def callFunc(str: String): js.Promise[js.Any] = js.native
//}

object JsTest extends IOApp.Simple {
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(formatter = LogFormatter.formatter, minimumLevel = Some(scribe.Level.Info))
    .replace()

  implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]

  def rb(
    fnName: String,
    air: String,
    args: List[(String, js.Any)]
  ): Future[Any] = {
    val peer = Fluence.getPeer()
    // result type in promise
    val pr: Promise[js.Any] = Promise[js.Any]()

    val rb = new RequestFlowBuilder()
    val relayPeerId = peer.getStatus().relayPeerId
    rb
      .disableInjections()
      .withRawScript(air)
      .configHandler((h, r) => {
        h.on("getDataSrv", "-relay-", (_, _) => { relayPeerId })
        args.foreach { (fnName, arg) =>
          h.on("getDataSrv", fnName, (_, _) => arg)
        }
        h.onEvent(
          "callbackSrv",
          "response",
          (args, _) => {
            println("RESPONSE: " + js.JSON.stringify(args))
            if (args.length == 1) {
              pr.success(args.pop())
            } else if (args.length == 0) {
              pr.success({})
            } else {
              pr.success(args)
            }
            {}
          }
        )
        h.onEvent(
          "errorHandlingSrv",
          "error",
          (args, _) => {

            println("ERROR HANDLING: " + js.JSON.stringify(args))
            pr.failure(new RuntimeException(args.pop().toString))
            {}
          }
        )
      })
      .handleScriptError((err) => {
        println("HANDLE SCRIPT ERROR: " + js.JSON.stringify(err))
        pr.failure(new RuntimeException("script error: " + err.toString))
      })
      .handleTimeout(() => {
        if (!pr.isCompleted) pr.failure(new RuntimeException(s"Request timed out for $fnName"))
      })

    peer.internals.initiateFlow(rb.build())

    pr.future
  }

  override def run: IO[Unit] =
    for {
      start <- IO(System.currentTimeMillis())
      _ <- IO {
        println(Path(".").absolute.toString)
      }
      sources = new AquaFileSources[IO](Path("./aqua/caller.aqua"), List())
      airV <- AquaCompiler
        .compile[IO, AquaFileError, FileModuleId, FileSpan.F](
          sources,
          SpanParser.parser,
          AirBackend,
          TransformConfig()
        )
      air = airV.getOrElse(Chain.empty)
      _ <- IO.fromFuture {
        IO {
          // ----- args from CLI -----
          val funcName = "callFunc"
          val args = ("str", "some result": js.Any) :: ("str2", "string two": js.Any) :: Nil
          val multiaddr = "/dns4/stage.fluence.dev/tcp/19002/wss/p2p/12D3KooWMigkP4jkVyufq5JnDJL6nXvyjeaDNpRfEZqQhsG3sYCU"
          // ----- args from CLI -----

          val z = air.toList
            .map(g => g.compiled.find(_.suffix.filter(f => f == funcName).isDefined))
            .flatten
          if (z.length > 1) {
            Future.failed(
              new RuntimeException(
                "something wrong, there is multiple number of functions with the same name"
              )
            )
          } else {
            z.headOption.map { func =>
              (for {
                _ <- Fluence
                  .start(multiaddr)
                  .toFuture
                result <- rb(
                  funcName,
                  func.content,
                  args
                )

              } yield {
                println("RESULTING: ")
                println("RESULT: " + result)
              })
            }.getOrElse(Future.failed(new RuntimeException("there is no such func")))
          }

        }
      }
      _ <- IO.println("Compilation ends in : " + (System.currentTimeMillis() - start) + " ms")
    } yield ()

}
