package aqua.run

import aqua.*
import aqua.ErrorRendering.showError
import aqua.backend.air.{AirBackend, FuncAirGen}
import aqua.backend.js.JavaScriptBackend
import aqua.backend.ts.TypeScriptBackend
import aqua.backend.{FunctionDef, Generated}
import aqua.builder.{ArgumentGetter, Finisher, ResultPrinter, Service}
import aqua.compiler.{AquaCompiled, AquaCompiler}
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.io.{AquaFileError, OutputPrinter}
import aqua.js.*
import aqua.model.{AquaContext, FuncArrow}
import aqua.model.transform.{Transform, TransformConfig}
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.LiteralToken
import aqua.parser.lift.FileSpan
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.run.RunConfig
import aqua.run.RunOpts.transformConfig
import aqua.types.*
import cats.data.*
import cats.effect.kernel.{Async, Clock}
import cats.effect.syntax.async.*
import cats.effect.{ExitCode, IO, IOApp, Resource, Sync}
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
  ): Future[KeyPair] = {
    sk.map { arr =>
      val typedArr = js.typedarray.Uint8Array.from(arr.map(_.toShort).toJSArray)
      KeyPair.fromEd25519SK(typedArr).toFuture
    }.getOrElse(KeyPair.randomEd25519().toFuture)
  }

  private def findFunction(contexts: Chain[AquaContext], funcName: String): Option[FuncArrow] =
    contexts
      .collectFirstSome(_.allFuncs.get(funcName))

  /**
   * Runs a function that is located in `input` file with FluenceJS SDK. Returns no output
   * @param func
   *   function name
   * @param input
   *   path to an aqua code with a function
   * @param imports
   *   the sources the input needs
   */
  def run[F[_]: Files: AquaIO: Async](
    func: CliFunc,
    input: Path,
    imports: List[Path],
    runConfig: RunConfig,
    transformConfig: TransformConfig
  ): F[ValidatedNec[String, Unit]] = {
    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]
    val funcCompiler = new FuncCompiler[F](input, imports, transformConfig, withRunImport = true)

    for {
      funcArrowV <- funcCompiler.compile(func)
      callResult <- Clock[F].timed {
        funcArrowV match {
          case Validated.Valid(funcCallable) =>
            val runner =
              new Runner(func, funcCallable, runConfig, transformConfig)
            runner.run()
          case i @ Validated.Invalid(_) => i.pure[F]
        }
      }
      (callTime, result) = callResult
    } yield {
      logger.debug(s"Call time: ${callTime.toMillis}ms")
      result
    }
  }

  private val builtinServices =
    aqua.builder.Console() :: aqua.builder.IPFSUploader("ipfs", "uploadFile") :: Nil

  /**
   * Executes a function with the specified settings
   * @param common
   *   common settings
   * @param funcName
   *   function name
   * @param inputPath
   *   path to a file with a function
   * @param imports
   *   imports that must be specified for correct compilation
   * @param args
   *   arguments to pass into a function
   * @param argumentGetters
   *   services to get argument if it is a variable
   * @param services
   *   will be registered before calling for correct execution
   * @return
   */
  def execRun[F[_]: Async](
    common: GeneralRunOptions,
    func: CliFunc,
    inputPath: Path,
    imports: List[Path] = Nil,
    argumentGetters: Map[String, VarJson] = Map.empty,
    services: List[Service] = Nil
  ): F[ValidatedNec[String, Unit]] = {
    LogFormatter.initLogger(Some(common.logLevel))
    implicit val aio: AquaIO[F] = new AquaFilesIO[F]
    RunCommand
      .run[F](
        func,
        inputPath,
        imports,
        RunConfig(common, argumentGetters, services ++ builtinServices),
        transformConfig(common.on, common.constants, common.flags.noXor, common.flags.noRelay)
      )
  }

}
