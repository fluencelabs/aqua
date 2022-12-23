package aqua.run

import aqua.*
import aqua.ErrorRendering.showError
import aqua.backend.air.{AirBackend, FuncAirGen}
import aqua.backend.js.JavaScriptBackend
import aqua.backend.ts.TypeScriptBackend
import aqua.backend.Generated
import aqua.logging.LogFormatter
import aqua.definitions.{FunctionDef, TypeDefinition}
import aqua.builder.{ArgumentGetter, Finisher, ResultPrinter, Service}
import aqua.compiler.{AquaCompiled, AquaCompiler}
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.io.{AquaFileError, AquaPath, OutputPrinter}
import aqua.js.*
import aqua.model.transform.{Transform, TransformConfig}
import aqua.model.{AquaContext, FuncArrow}
import aqua.parser.expr.func.CallArrowExpr
import aqua.parser.lexer.LiteralToken
import aqua.parser.lift.FileSpan
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.run.RunConfig
import aqua.run.RunOpts.transformConfig
import aqua.types.*
import cats.data.*
import cats.effect.*
import cats.effect.kernel.{Async, Clock}
import cats.effect.syntax.async.*
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.list.*
import cats.syntax.monad.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import cats.{Id, Monad, ~>}
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

  private def createGetter(value: VarRaw, arg: js.Dynamic, argType: Type): ArgumentGetter = {
    val converted = Conversions.ts2aqua(arg, TypeDefinitionJs(TypeDefinition(argType)))
    ArgumentGetter(value.copy(baseType = argType), converted)
  }

  // Creates getter services for variables. Return an error if there is no variable in services
  // and type of this variable couldn't be optional
  private def getGettersForVars(
    vars: List[(String, Type)],
    argGetters: Map[String, VarJson]
  ): ValidatedNec[String, List[ArgumentGetter]] = {
    vars.map { (n, argType) =>
      val argGetterOp = argGetters.get(n)
      (argGetterOp, argType) match {
        case (None, _) => Validated.invalidNec(s"Unexcepted. There is no service for '$n' argument")
        // BoxType could be undefined, so, pass service that will return 'undefined' for this argument
        case (Some(s), _: BoxType) if s._2 == js.undefined =>
          Validated.validNec(createGetter(s._1, s._2, argType) :: Nil)
        case (Some(s), _) if s._2 == js.undefined =>
          Validated.invalidNec(
            s"Argument '$n' is missing. Expected argument '$n' of type '$argType'"
          )
        case (Some(s), _) =>
          Validated.validNec(createGetter(s._1, s._2, argType) :: Nil)
      }
    }.reduceOption(_ combine _).getOrElse(Validated.validNec(Nil))
  }

  def resultVariableNames(funcCallable: FuncArrow, name: String): List[String] =
    funcCallable.arrowType.codomain.toList.zipWithIndex.map { case (t, idx) =>
      name + idx
    }

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
    input: Option[AquaPath],
    imports: List[Path],
    runConfig: RunConfig,
    // services that will pass arguments to air
    argumentGetters: Map[String, VarJson],
    // builtin services for aqua run, for example: Console, FileSystem, etc
    services: List[Service],
    jsonServices: List[JsonService],
    plugins: List[String],
    transformConfig: TransformConfig
  ): F[ValidatedNec[String, Unit]] = {
    val funcCompiler = new FuncCompiler[F](input, imports, transformConfig, withRunImport = true)

    for {
      contextV <- funcCompiler.compile(true)
      callResult <- Clock[F].timed {
        contextV.andThen { context =>
          FuncCompiler
            .findFunction(context, func)
            .andThen(callable =>
              JsonService
                .findServices(context, jsonServices)
                .map(jsonServices => (callable, jsonServices))
            )
        }.andThen { case (funcCallable, jsonServices) =>
          val resultNames = resultVariableNames(funcCallable, runConfig.resultName)
          val resultPrinterService =
            ResultPrinter(
              runConfig.resultPrinterServiceId,
              runConfig.resultPrinterName,
              resultNames
            )
          val promiseFinisherService =
            Finisher(runConfig.finisherServiceId, runConfig.finisherFnName)

          val vars = func.args
            .zip(funcCallable.arrowType.domain.toList)
            .collect { case (VarRaw(n, _), argType) =>
              (n, argType)
            }
            .distinctBy(_._1)
          getGettersForVars(vars, argumentGetters).andThen { getters =>
            val gettersTags = getters.map(s => s.callTag())
            val preparer =
              new CallPreparer(
                func,
                funcCallable,
                gettersTags,
                resultPrinterService.callTag,
                promiseFinisherService.callTag(),
                runConfig,
                transformConfig
              )
            preparer.prepare().map { info =>
              FuncCaller.funcCall[F](
                info.name,
                info.air,
                info.definitions,
                info.config,
                resultPrinterService,
                promiseFinisherService,
                services ++ jsonServices,
                getters,
                plugins
              )
            }

          }
        } match {
          case Validated.Valid(f) =>
            f
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
    aqua.builder
      .Console() :: aqua.builder.IPFSUploader("ipfs") :: aqua.builder.DeployHelper() :: Nil

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
    runInfo: RunInfo
  ): F[ValidatedNec[String, Unit]] = {
    val common = runInfo.common
    LogFormatter.initLogger(Some(common.logLevel.compiler))
    implicit val aio: AquaIO[F] = new AquaFilesIO[F]

    RunCommand
      .run[F](
        runInfo.func,
        runInfo.input,
        runInfo.imports,
        RunConfig(
          common
        ),
        runInfo.argumentGetters,
        runInfo.services ++ builtinServices,
        runInfo.jsonServices,
        runInfo.pluginsPaths,
        transformConfig(common.on, common.constants, common.flags.noXor, common.flags.noRelay)
      )
  }

}
