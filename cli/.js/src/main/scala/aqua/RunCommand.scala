package aqua

import aqua.backend.Generated
import aqua.backend.air.AirBackend
import aqua.backend.js.JavaScriptBackend
import aqua.backend.ts.TypeScriptBackend
import aqua.compiler.{AquaCompiled, AquaCompiler}
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.io.AquaFileError
import aqua.model.transform.TransformConfig
import aqua.model.transform.res.FuncRes
import aqua.parser.expr.CallArrowExpr
import aqua.parser.lexer.Literal
import aqua.parser.lift.FileSpan
import cats.data.*
import cats.effect.kernel.Async
import cats.effect.syntax.async.*
import cats.effect.{IO, IOApp, Sync}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monad.*
import cats.syntax.show.*
import cats.{Id, Monad, ~>}
import fs2.io.file.{Files, Path}
import scribe.Logging

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.annotation.*

object RunCommand extends Logging {

  def funcCall(multiaddr: String, funcName: String, air: Generated)(implicit ec: ExecutionContext): Future[Validated[String, Unit]] = {
    (for {
      _ <- Fluence
        .start(multiaddr)
        .toFuture
      peer = Fluence.getPeer()
      _ = CallJsFunction.registerUnitService(peer, "console", "print", args => println("print: " + args))
      result <- CallJsFunction.funcCallJs(
        peer,
        funcName,
        air.content,
        Nil,
        None // TODO
      )
      _ <- peer.stop().toFuture
    } yield {
      Validated.Valid({})
    })
  }

  val generatedFuncName = "callerUniqueFunction"

  def run[F[_]: Monad: Files: AquaIO: Async](multiaddr: String, func: String, input: Path, imps: List[Path])(implicit ec: ExecutionContext): F[Unit] = {
    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]
    for {
      start <- System.currentTimeMillis().pure[F]
      _ = println("Started at: " + start + " ms")
      generatedFile = Path("./.aqua/call0.aqua").absolute
      absInput = input.absolute
      code =
        s"""import "${absInput.toString}"
           |
           |func $generatedFuncName():
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
      parsingTime = System.currentTimeMillis()
      _ = println("Parsing ends in : " + (parsingTime - start) + " ms")
      result <- {
        airV match {
          case Validated.Valid(airC: Chain[AquaCompiled[FileModuleId]]) =>
            // Cause we generate input with only one function, we should have only one air compiled content
            airC.headOption.flatMap(_.compiled.headOption).map { air =>
              Async[F].fromFuture {
                funcCall(multiaddr, generatedFuncName, air).map(_.toValidatedNec).pure[F]
              }
            }.getOrElse {
              Validated.invalidNec("Unexpected. There could be only one generated function.").pure[F]
            }
          case Validated.Invalid(errs) =>
            import ErrorRendering.showError
            Validated.invalid(errs.map(_.show)).pure[F]

        }
      }

      _ = println("Function call ends in : " + (System.currentTimeMillis() - parsingTime) + " ms")
    } yield {
      result.fold({ (errs: NonEmptyChain[String]) =>
        errs.toChain.toList.foreach(err => println(err + "\n"))
      }, identity)
    }
  }

}
