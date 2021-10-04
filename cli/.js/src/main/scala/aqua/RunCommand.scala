package aqua

import aqua.backend.air.AirBackend
import aqua.backend.js.JavaScriptBackend
import aqua.backend.ts.TypeScriptBackend
import aqua.compiler.{AquaCompiled, AquaCompiler}
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.io.AquaFileError
import aqua.model.transform.TransformConfig
import aqua.model.transform.res.FuncRes
import aqua.parser.expr.CallArrowExpr
import aqua.parser.lift.FileSpan
import cats.Monad
import cats.data.Validated
import cats.effect.{IO, IOApp, Sync}
import fs2.io.file.{Files, Path}
import cats.data.Chain
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.monad.*
import cats.syntax.functor.*
import cats.~>
import cats.Id
import aqua.parser.lexer.Literal

import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.annotation.*

trait ParticleContext {
  def particleId: String
  def initPeerId: String
  def timestamp: Int
  def ttl: Int
  def signature: String
}

object ResultCodes {
  val success = 0
  val unknownError = 1
  val exceptionInHandler = 2
}

trait CallServiceResult extends js.Object {
  def retCode: Int
  def retCode_=(code: Int): Unit
  def result: js.Any
  def result_=(res: js.Any): Unit
}

trait CallServiceData extends js.Object {
  def serviceId: String
  def fnName: String
  def args: js.Array[js.Any]
  def particleContext: ParticleContext
  def tetraplets: js.Any
}

trait Internals extends js.Object {
  def initiateFlow(r: RequestFlow): Promise[js.Any]
  def callServiceHandler: CallServiceHandler
}

trait Status extends js.Object {
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

  def use(f: js.Function3[CallServiceData, CallServiceResult, js.Function0[Unit], Unit]): CallServiceHandler = js.native
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

object RunCommand {
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(formatter = LogFormatter.formatter, minimumLevel = Some(scribe.Level.Info))
    .replace()

  def registerService(peer: FluencePeer, serviceId: String, fnName: String, f: (js.Array[js.Any]) => Unit) = {
    peer.internals.callServiceHandler.use((req, resp, next) => {
      if (req.serviceId == serviceId && req.fnName == fnName) {
        f(req.args)
        resp.retCode = ResultCodes.success
        resp.result = new js.Object {}
      }

      next()
    })
  }

  def rb(
    fnName: String,
    air: String,
    args: List[(String, js.Any)],
    funcRes: FuncRes
  ): Future[Any] = {
    val peer = Fluence.getPeer()
    val pr: Promise[js.Any] = Promise[js.Any]()

    val rb = new RequestFlowBuilder()
    val relayPeerId = peer.getStatus().relayPeerId

    registerService(peer, "console", "print", args => println("print: " + args))

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

    funcRes.returnType.fold(Future.unit)(_ => pr.future)
  }

  val funcName = "myRandomFunc"

  def someFuture(multiaddr: String, air: Chain[AquaCompiled[FileModuleId]]) = {
    val z = air.toList
      .map(g => g.compiled.find(_.func.map(_.funcName).filter(f => f == funcName).isDefined))
      .flatten
    if (z.length > 1) {
      Future.failed(
        new RuntimeException(
          "something wrong, there is multiple number of functions with the same name"
        )
      )
    } else {
      z.headOption.map { gen =>
        gen.func match {
          case Some(f) =>
            (for {
              _ <- Fluence
                .start(multiaddr)
                .toFuture
              result <- rb(
                funcName,
                gen.content,
                Nil,
                f
              )

            } yield {
            })
          case None =>
            Future.failed(new RuntimeException("something wrong, no FuncRes"))
        }

      }.getOrElse(Future.failed(new RuntimeException("there is no such func")))
    }
  }

  def run[F[_]: Monad: Files: AquaIO](multiaddr: String, func: String)(implicit F: Future ~> F): F[Unit] = {
    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]
    for {
      start <- System.currentTimeMillis().pure[F]
      _ = println("run on " + start)
      input = Path("./aqua/caller.aqua").absolute // should be faked
      generatedFile = Path("./.aqua/call0.aqua").absolute
      code =
        s"""import "${input.toString}"
           |
           |func $funcName():
           |  $func
           |""".stripMargin
      _ <- AquaIO[F].writeFile(generatedFile, code).value
      imports = input :: Nil // should be input
      sources = new AquaFileSources[F](generatedFile, imports)
      airV <- AquaCompiler
        .compile[F, AquaFileError, FileModuleId, FileSpan.F](
          sources,
          SpanParser.parser,
          AirBackend,
          TransformConfig()
        )
      _ = println("Parsing ends in : " + (System.currentTimeMillis() - start) + " ms")
      air: Chain[AquaCompiled[FileModuleId]] = airV.getOrElse(Chain.empty)
      _ <- F {
        someFuture(multiaddr, air)
      }

      _ = println("Compilation ends in : " + (System.currentTimeMillis() - start) + " ms")
    } yield ()
  }

}
