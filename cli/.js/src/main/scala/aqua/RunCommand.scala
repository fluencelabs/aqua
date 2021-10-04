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

  def funcCallJs(
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

  def funcCall(multiaddr: String, air: Chain[AquaCompiled[FileModuleId]]) = {
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
              result <- funcCallJs(
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

  def run[F[_]: Monad: Files: AquaIO](multiaddr: String, func: String, input: Path, imps: List[Path])(implicit F: Future ~> F): F[Unit] = {
    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]
    for {
      start <- System.currentTimeMillis().pure[F]
      _ = println("run on " + start)
      generatedFile = Path("./.aqua/call0.aqua").absolute
      absInput = input.absolute
      code =
        s"""import "${absInput.toString}"
           |
           |func $funcName():
           |  $func
           |""".stripMargin
      _ <- AquaIO[F].writeFile(generatedFile, code).value
      imports = absInput +: imps.map(_.absolute)
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
        funcCall(multiaddr, air)
      }

      _ = println("Compilation ends in : " + (System.currentTimeMillis() - start) + " ms")
    } yield ()
  }

}
