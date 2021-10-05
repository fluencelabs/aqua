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
import cats.syntax.show.*
import cats.~>
import cats.Id
import aqua.parser.lexer.Literal
import cats.data.{NonEmptyList, NonEmptyChain}
import cats.data.ValidatedNel

import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.concurrent.ExecutionContext
import scribe.Logging

object RunCommand extends Logging {
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
    val resultPromise: Promise[js.Any] = Promise[js.Any]()

    val requestBuilder = new RequestFlowBuilder()
    val relayPeerId = peer.getStatus().relayPeerId

    registerService(peer, "console", "print", args => println("print: " + args))

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
        if (!resultPromise.isCompleted) resultPromise.failure(new RuntimeException(s"Request timed out for $fnName"))
      })

    peer.internals.initiateFlow(requestBuilder.build())

    funcRes.returnType.fold(Future.unit)(_ => resultPromise.future)
  }

  val funcName = "callerUniqueFunction"

  def funcCall(multiaddr: String, air: Chain[AquaCompiled[FileModuleId]])(implicit ec: ExecutionContext): Future[Validated[String, Unit]] = {

    val z = air.toList
      .map(g => g.compiled.find(_.func.map(_.funcName).filter(f => f == funcName).isDefined))
      .flatten
    if (z.length > 1) {
      Future.successful(
        Validated.Invalid("Unexpected. There is multiple number of functions with the same name")
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
              Validated.Valid({})
            })
          case None =>
            Future.successful(Validated.Invalid("Unexpected. No FuncRes in generated object"))
        }

      }.getOrElse(Future.successful(Validated.Invalid("Unexpected. Cannot find ")))
    }
  }

  def run[F[_]: Monad: Files: AquaIO](multiaddr: String, func: String, input: Path, imps: List[Path])(implicit F: Future ~> F, ec: ExecutionContext): F[Unit] = {
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
      result <- {
        airV match {
          case Validated.Valid(air) =>
            F {
              funcCall(multiaddr, air).map(_.toValidatedNec)
            }
          case Validated.Invalid(errs) =>
            import ErrorRendering.showError
            Validated.invalid(errs.map(_.show)).pure[F]

        }
      }

      _ = println("Compilation ends in : " + (System.currentTimeMillis() - start) + " ms")
    } yield {
      result.fold({ (errs: NonEmptyChain[String]) =>
        errs.toChain.toList.foreach(err => println(err + "\n"))
      }, identity)
    }
  }

}
