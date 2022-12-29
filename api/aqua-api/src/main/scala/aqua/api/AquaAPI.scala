package aqua.api

import aqua.ErrorRendering.showError
import aqua.api.types.{AquaAPIConfig, AquaConfig, AquaFunction, CompilationResult, Input}
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
import cats.data.Validated.{Invalid, Valid, invalidNec, validNec}
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
import scala.scalajs.js.|
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.*
import scala.scalajs.js.{UndefOr, undefined}
import aqua.js.{FunctionDefJs, ServiceDefJs, VarJson}
import aqua.model.AquaContext
import aqua.raw.ops.CallArrowRawTag
import aqua.raw.value.{LiteralRaw, VarRaw}
import aqua.res.AquaRes
import cats.Applicative

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
  def compile(
    input: types.Input | types.Path | types.Call,
    imports: js.Array[String],
    aquaConfigJS: js.UndefOr[AquaConfig]
  ) = {
    val aquaConfig: AquaAPIConfig =
      aquaConfigJS.toOption.map(cjs => AquaAPIConfig.fromJS(cjs)).getOrElse(AquaAPIConfig())

    val importsList = imports.toList

    input match {
      case i: types.Input => compileString(i.input, importsList, aquaConfig)
      case p: types.Path => compilePath(p.path, importsList, aquaConfig)
      case c: types.Call =>
        val path = c.input match {
          case i: types.Input => i.input
          case p: types.Path => p.path
        }
        compileCall(c.functionCall, c.arguments, path, importsList, aquaConfig)

    }
  }

  def compileCall(
    functionStr: String,
    arguments: js.Dynamic,
    pathStr: String,
    imports: List[String],
    aquaConfig: AquaAPIConfig
  ): js.Promise[CompilationResult] = {
    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]
    (
      LogLevels.levelFromString(aquaConfig.logLevel),
      Constants.parse(aquaConfig.constants)
    ).mapN { (level, constants) =>

      val transformConfig = aquaConfig.getTransformConfig.copy(constants = constants)

      LogFormatter.initLogger(Some(level))

      new FuncCompiler[IO](
        Some(RelativePath(Path(pathStr))),
        imports.toList.map(Path.apply),
        transformConfig
      ).compile()
        .map { contextV =>
          contextV.andThen { context =>
            CliFunc
              .fromString(functionStr)
              .leftMap(errs => NonEmptyChain.fromNonEmptyList(errs))
              .andThen { cliFunc =>
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
        }
        .flatMap {
          case Valid(result) => IO.pure(CompilationResult.result(js.Dictionary(), js.Dictionary(), Some(result)))
          case Invalid(err) =>
            val errs = err.map(_.show).distinct.toChain.toList
            IO.pure(CompilationResult.errs(errs))
        }
        .unsafeToFuture()
        .toJSPromise
    } match {
      case Valid(pr) => pr
      case Invalid(err) => js.Promise.resolve(CompilationResult.errs(err.toList))
    }

  }

  def compilePath(
    pathStr: String,
    imports: List[String],
    aquaConfig: AquaAPIConfig
  ): js.Promise[CompilationResult] = {
    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]
    val path = Path(pathStr)
    val sources = new AquaFileSources[IO](path, imports.map(Path.apply))
    compileRaw(aquaConfig, sources)
  }

  def compileString(
    input: String,
    imports: List[String],
    aquaConfig: AquaAPIConfig
  ): js.Promise[CompilationResult] = {
    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]
    val path = Path("")

    val strSources: AquaFileSources[IO] =
      new AquaFileSources[IO](path, imports.map(Path.apply)) {
        override def sources: IO[ValidatedNec[AquaFileError, Chain[(FileModuleId, String)]]] = {
          IO.pure(Valid(Chain.one((FileModuleId(path), input))))
        }
      }
    compileRaw(aquaConfig, strSources)
  }

  def compileRaw(
    aquaConfig: AquaAPIConfig,
    sources: AquaSources[IO, AquaFileError, FileModuleId]
  ): js.Promise[CompilationResult] = {

    (
      LogLevels.levelFromString(aquaConfig.logLevel),
      Constants.parse(aquaConfig.constants)
    ).mapN { (level, constants) =>

      LogFormatter.initLogger(Some(level))

      val config = AquaCompilerConf(constants)
      val transformConfig = aquaConfig.getTransformConfig

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
            val serviceDefs = allGenerated.flatMap(_.services).map(s => ServiceDefJs(s))
            val functions = allGenerated.flatMap(
              _.air.map(as => (as.name, AquaFunction(FunctionDefJs(as.funcDef), as.air)))
            )

            IO.pure(
              CompilationResult.result(
                js.Dictionary.apply(serviceDefs.map(s => s.name -> s): _*),
                js.Dictionary.apply(functions: _*),
                None
              )
            )
          case Invalid(errChain) =>
            val errors = errChain.map(_.show).distinct.toChain.toList
            IO.pure[CompilationResult](CompilationResult.errs(errors))
        }
      } yield {
        jsResult
      }

      proc.unsafeToFuture().toJSPromise
    } match {
      case Valid(pr) => pr
      case Invalid(err) => js.Promise.resolve(CompilationResult.errs(err.toList))
    }
  }
}
