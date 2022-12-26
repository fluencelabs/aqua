package aqua.api

import aqua.ErrorRendering.showError
import aqua.backend.{AirFunction, Backend, Generated}
import aqua.compiler.*
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.logging.{LogFormatter, LogLevels}
import aqua.constants.Constants
import aqua.io.*
import aqua.raw.ops.Call
import aqua.run.{CallInfo, CallPreparer, CliFunc, FuncCompiler, RunPreparer}
import aqua.parser.lexer.{LiteralToken, Token}
import aqua.parser.lift.FileSpan.F
import aqua.parser.lift.{FileSpan, Span}
import aqua.parser.{ArrowReturnError, BlockIndentError, LexerError, ParserError}
import aqua.semantics.{CompilerState, HeaderError, RulesViolated, WrongAST}
import aqua.{AquaIO, SpanParser}
import aqua.model.transform.{Transform, TransformConfig}
import aqua.backend.api.APIBackend
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.data.Validated.{invalidNec, validNec, Invalid, Valid}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.show.*
import cats.syntax.traverse.*
import fs2.io.file.{Files, Path}
import scribe.Logging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.*
import scala.scalajs.js.{undefined, UndefOr}
import aqua.js.{FunctionDefJs, ServiceDefJs, VarJson}
import aqua.model.AquaContext
import aqua.raw.ops.CallArrowRawTag
import aqua.raw.value.{LiteralRaw, VarRaw}
import aqua.res.AquaRes
import cats.Applicative

@JSExportTopLevel("AquaFunction")
case class AquaFunction(
  @JSExport
  funcDef: FunctionDefJs,
  @JSExport
  script: String
)

case class AquaAPIConfig(
  logLevel: String = "info",
  constants: List[String] = Nil,
  noXor: Boolean = false,
  noRelay: Boolean = false
)

object AquaAPIConfig {

  def fromJS(cjs: AquaConfig): AquaAPIConfig = {
    AquaAPIConfig(
      cjs.logLevel.getOrElse("info"),
      cjs.constants.map(_.toList).getOrElse(Nil),
      cjs.noXor.getOrElse(false),
      cjs.noRelay.getOrElse(false)
    )
  }
}

@JSExportTopLevel("AquaConfig")
case class AquaConfig(
  @JSExport
  logLevel: js.UndefOr[String],
  @JSExport
  constants: js.UndefOr[js.Array[String]],
  @JSExport
  noXor: js.UndefOr[Boolean],
  @JSExport
  noRelay: js.UndefOr[Boolean]
)

@JSExportTopLevel("CompilationResult")
case class CompilationResult(
  @JSExport
  services: js.Array[ServiceDefJs],
  @JSExport
  functions: js.Dictionary[AquaFunction]
)

@JSExportTopLevel("Aqua")
object AquaAPI extends App with Logging {

  def getTag(serviceId: String, value: VarRaw) = {
    CallArrowRawTag.service(
      LiteralRaw.quote(serviceId),
      value.name,
      Call(List.empty, List(Call.Export(value.name, value.baseType)))
    )
  }

  @JSExport
  def compileRun(
    functionStr: String,
    arguments: js.Dynamic,
    pathStr: String,
    imports: js.Array[String],
    aquaConfigJS: js.UndefOr[AquaConfig]
  ): js.Promise[AquaFunction] = {
    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]
    val aquaConfig: AquaAPIConfig =
      aquaConfigJS.toOption.map(cjs => AquaAPIConfig.fromJS(cjs)).getOrElse(AquaAPIConfig())
    LogFormatter.initLogger(Some(LogLevels.levelFromString(aquaConfig.logLevel).toOption.get))
    val transformConfig = TransformConfig()

    new FuncCompiler[IO](
      Some(RelativePath(Path(pathStr))),
      imports.toList.map(Path.apply),
      transformConfig
    ).compile(false).map { contextV =>
      contextV.andThen { context =>
        CliFunc.fromString(functionStr).leftMap(errs => NonEmptyChain.fromNonEmptyList(errs)).andThen { cliFunc =>
          FuncCompiler.findFunction(context, cliFunc).andThen { arrow =>
            VarJson.checkDataGetServices(cliFunc.args, Some(arguments)).andThen {
              case (argsWithTypes, _) =>
                val func = cliFunc.copy(args = argsWithTypes)
                val preparer = new RunPreparer(
                  func,
                  arrow,
                  transformConfig
                )
                preparer.prepare().map { ci =>
                  AquaFunction(FunctionDefJs(ci.definitions), ci.air)
                }
            }
          }
        }
      }
    }.flatMap {
      case Valid(result) => IO.pure(result)
      case Invalid(err) =>
        println(err)
        err.map(_.show).distinct.map(OutputPrinter.errorF[IO]).sequence
        IO.raiseError[AquaFunction](new Error("Compilation failed."))
    }.unsafeToFuture().toJSPromise

  }

  @JSExport
  def compile(
    pathStr: String,
    imports: js.Array[String],
    aquaConfigJS: js.UndefOr[AquaConfig]
  ): js.Promise[CompilationResult] = {
    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]
    val path = Path(pathStr)
    val sources = new AquaFileSources[IO](path, imports.toList.map(Path.apply))
    compileRaw(aquaConfigJS, sources)
  }

  @JSExport
  def compileString(
    input: String,
    imports: js.Array[String],
    aquaConfigJS: js.UndefOr[AquaConfig]
  ): js.Promise[CompilationResult] = {
    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]
    val path = Path("")
    val strSources: AquaFileSources[IO] =
      new AquaFileSources[IO](path, imports.toList.map(Path.apply)) {
        override def sources: IO[ValidatedNec[AquaFileError, Chain[(FileModuleId, String)]]] = {
          IO.pure(Valid(Chain.one((FileModuleId(path), input))))
        }
      }
    compileRaw(aquaConfigJS, strSources)
  }

  def compileRaw(
    aquaConfigJS: js.UndefOr[AquaConfig],
    sources: AquaSources[IO, AquaFileError, FileModuleId]
  ): js.Promise[CompilationResult] = {
    val aquaConfig =
      aquaConfigJS.toOption.map(cjs => AquaAPIConfig.fromJS(cjs)).getOrElse(AquaAPIConfig())

    (
      LogLevels.levelFromString(aquaConfig.logLevel),
      Constants.parse(aquaConfig.constants)
    ).mapN { (level, constants) =>

      LogFormatter.initLogger(Some(level))

      val config = AquaCompilerConf(constants)
      val transformConfig = TransformConfig()

      val proc = for {
        res <- CompilerAPI
          .compile[IO, AquaFileError, FileModuleId, FileSpan.F](
            sources,
            SpanParser.parser,
            new AirValidator[IO] {
              override def init(): IO[Unit] = Applicative[IO].pure(())
              override def validate(airs: List[AirFunction]): IO[ValidatedNec[String, Unit]] =
                Applicative[IO].pure(validNec(()))
            },
            new Backend.Transform:
              override def transform(ex: AquaContext): AquaRes =
                Transform.contextRes(ex, transformConfig)

              override def generate(aqua: AquaRes): Seq[Generated] = APIBackend.generate(aqua)
            ,
            config
          )
        jsResult <- res match {
          case Valid(compiled) =>
            val allGenerated: List[Generated] = compiled.toList.flatMap(_.compiled)
            val serviceDefs = allGenerated.flatMap(_.services).map(s => ServiceDefJs(s)).toJSArray
            val functions = allGenerated.flatMap(
              _.air.map(as => (as.name, AquaFunction(FunctionDefJs(as.funcDef), as.air)))
            )

            IO.pure(CompilationResult(serviceDefs, js.Dictionary.apply(functions: _*)))
          case Invalid(errChain) =>
            errChain.map(_.show).distinct.map(OutputPrinter.errorF[IO]).sequence
            IO.raiseError[CompilationResult](new Error("Compilation failed."))
        }
      } yield {
        jsResult
      }

      proc.unsafeToFuture().toJSPromise
    } match {
      case Valid(pr) => pr
      case Invalid(err) => js.Promise.reject(err)
    }
  }
}
