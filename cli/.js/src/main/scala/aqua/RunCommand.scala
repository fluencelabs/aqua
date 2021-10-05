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
import aqua.parser.lexer.Literal
import aqua.parser.lift.FileSpan
import cats.data.*
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

  def funcCall(multiaddr: String, funcName: String, air: Chain[AquaCompiled[FileModuleId]])(implicit ec: ExecutionContext): Future[Validated[String, Unit]] = {

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
          case Some(funcRes) =>
            (for {
              _ <- Fluence
                .start(multiaddr)
                .toFuture
              peer = Fluence.getPeer()
              _ = CallJsFunction.registerUnitService(peer, "console", "print", args => println("print: " + args))
              result <- CallJsFunction.funcCallJs(
                peer,
                funcName,
                gen.content,
                Nil,
                funcRes
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

  val generatedFuncName = "callerUniqueFunction"

  def run[F[_]: Monad: Files: AquaIO](multiaddr: String, func: String, input: Path, imps: List[Path])(implicit F: Future ~> F, ec: ExecutionContext): F[Unit] = {
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
          case Validated.Valid(air) =>
            F {
              funcCall(multiaddr, generatedFuncName, air).map(_.toValidatedNec)
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
