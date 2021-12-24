package aqua.run

import aqua.*
import aqua.ErrorRendering.showError
import aqua.backend.air.{AirBackend, FuncAirGen}
import aqua.backend.js.JavaScriptBackend
import aqua.backend.ts.TypeScriptBackend
import aqua.backend.{FunctionDef, Generated}
import aqua.builder.{Console, Finisher}
import aqua.compiler.{AquaCompiled, AquaCompiler}
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.io.{AquaFileError, OutputPrinter}
import aqua.js.*
import aqua.model.func.raw.{CallArrowTag, CallServiceTag, FuncOp, FuncOps}
import aqua.model.func.{Call, FuncCallable}
import aqua.model.transform.res.{AquaRes, FuncRes}
import aqua.model.transform.{Transform, TransformConfig}
import aqua.model.{AquaContext, LiteralModel, ValueModel, VarModel}
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.Literal
import aqua.parser.lift.FileSpan
import aqua.run.RunConfig
import aqua.types.*
import cats.data.*
import cats.effect.kernel.{Async, Clock}
import cats.effect.syntax.async.*
import cats.effect.{IO, IOApp, Resource, Sync}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.list.*
import cats.syntax.monad.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import cats.{~>, Id, Monad}
import fs2.io.file.{Files, Path}
import scribe.Logging

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.JSON
import scala.scalajs.js.annotation.*

object RunCommand extends Logging {

  def createKeyPair(
    sk: Option[Array[Byte]]
  )(implicit ec: ExecutionContext): Future[Option[KeyPair]] = {
    sk.map { arr =>
      val typedArr = js.typedarray.Uint8Array.from(arr.map(_.toShort).toJSArray)
      KeyPair.fromEd25519SK(typedArr).toFuture.map(Some.apply)
    }.getOrElse(Future.successful(None))
  }

  private def findFunction(contexts: Chain[AquaContext], funcName: String): Option[FuncCallable] =
    contexts
      .flatMap(_.exports.map(e => Chain.fromSeq(e.funcs.values.toList)).getOrElse(Chain.empty))
      .find(_.funcName == funcName)

  /**
   * Runs a function that is located in `input` file with FluenceJS SDK. Returns no output
   * @param func function name
   * @param input path to an aqua code with a function
   * @param imports the sources the input needs
   */
  def run[F[_]: Files: AquaIO: Async](
    func: String,
    args: List[ValueModel],
    input: Path,
    imports: List[Path],
    runConfig: RunConfig,
    transformConfig: TransformConfig = TransformConfig()
  )(implicit ec: ExecutionContext): F[Unit] = {
    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]

    for {
      prelude <- Prelude.init()
      sources = new AquaFileSources[F](input, prelude.importPaths)
      // compile only context to wrap and call function later
      compileResult <- Clock[F].timed(
        AquaCompiler
          .compileToContext[F, AquaFileError, FileModuleId, FileSpan.F](
            sources,
            SpanParser.parser,
            transformConfig
          )
          .map(_.leftMap(_.map(_.show)))
      )
      (compileTime, contextV) = compileResult
      callResult <- Clock[F].timed {
        val resultV: ValidatedNec[String, F[Unit]] = contextV.andThen { contextC =>
          findFunction(contextC, func) match {
            case Some(funcCallable) =>
              val runner =
                new Runner(func, funcCallable, args, runConfig, transformConfig)
              runner.run()
            case None =>
              Validated.invalidNec[String, F[Unit]](s"There is no function called '$func'")
          }
        }
        resultV.sequence
      }
      (callTime, result) = callResult
    } yield {
      logger.debug(s"Compile time: ${compileTime.toMillis}ms")
      logger.debug(s"Call time: ${callTime.toMillis}ms")
      result.fold(
        { (errs: NonEmptyChain[String]) =>
          errs.toChain.toList.foreach(err => OutputPrinter.error(err + "\n"))
        },
        identity
      )
    }
  }

}
