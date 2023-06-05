package api

import api.types.{AquaConfig, AquaFunction, CompilationResult, GeneratedSource, Input}
import aqua.ErrorRendering.showError
import aqua.raw.value.ValueRaw
import aqua.api.{APICompilation, AquaAPIConfig}
import aqua.api.TargetType.*
import aqua.backend.air.AirBackend
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
import aqua.backend.js.JavaScriptBackend
import aqua.backend.ts.TypeScriptBackend
import aqua.definitions.FunctionDef
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
import scala.scalajs.js.{|, undefined, Promise, UndefOr}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.*
import aqua.js.{FunctionDefJs, ServiceDefJs, VarJson}
import aqua.model.AquaContext
import aqua.raw.ops.CallArrowRawTag
import aqua.raw.value.{LiteralRaw, VarRaw}
import aqua.res.AquaRes
import cats.Applicative

@JSExportTopLevel("Aqua")
object AquaAPI extends App with Logging {

  /**
   * All-in-one function that support different inputs and backends
   * @param input can be a path to aqua file, string with a code or a function call
   * @param imports list of paths
   * @param aquaConfigJS compiler config
   * @return compiler results depends on input and config
   */
  @JSExport
  def compile(
    input: types.Input | types.Path | types.Call,
    imports: js.Array[String],
    aquaConfigJS: js.UndefOr[AquaConfig]
  ): Promise[CompilationResult] = {
    aquaConfigJS.toOption
      .map(cjs => AquaConfig.fromJS(cjs))
      .getOrElse(
        validNec(AquaAPIConfig())
      ).map { config =>
      val importsList = imports.toList

      input match {
        case i: (types.Input | types.Path) =>
          compileAll(i, importsList, config)
        case c: types.Call =>
          compileCall(c, importsList, config)

      }
    } match {
      case Valid(v) => v.unsafeToFuture().toJSPromise
      case Invalid(errs) => js.Promise.resolve(CompilationResult.errs(errs.toChain.toList))
    }
  }

  // Compile all non-call inputs
  private def compileAll(
    input: types.Input | types.Path,
    imports: List[String],
    config: AquaAPIConfig
  ): IO[CompilationResult] = {
    val backend: Backend = config.targetType match {
      case AirType => APIBackend
      case TypeScriptType => TypeScriptBackend()
      case JavaScriptType => JavaScriptBackend()
    }

    extension (res: IO[ValidatedNec[String, Chain[AquaCompiled[FileModuleId]]]])
      def toResult: IO[CompilationResult] = res.map(
        _.andThen { compiled =>
          config.targetType match {
            case AirType => validNec(generatedToAirResult(compiled))
            case TypeScriptType => compiledToTsSourceResult(compiled)
            case JavaScriptType => compiledToJsSourceResult(compiled)
          }
        }.leftMap(errorsToResult).merge
      )

    input match {
      case i: types.Input =>
        APICompilation
          .compileString(
            i.input,
            imports,
            config,
            backend
          )
          .toResult
      case p: types.Path =>
        APICompilation
          .compilePath(
            p.path,
            imports,
            config,
            backend
          )
          .toResult
    }

  }

  private def compileCall(call: types.Call, imports: List[String], config: AquaAPIConfig) = {
    val path = call.input match {
      case i: types.Input => i.input
      case p: types.Path => p.path
    }

    extension (res: IO[ValidatedNec[String, (FunctionDef, String)]])
      def callToResult: IO[CompilationResult] = res.map(
        _.map { case (definitions, air) =>
          CompilationResult.result(call = Some(AquaFunction(FunctionDefJs(definitions), air)))
        }.leftMap(errorsToResult).merge
      )

    APICompilation
      .compileCall(
        call.functionCall,
        path,
        imports,
        config,
        vr => VarJson.checkDataGetServices(vr, Some(call.arguments)).map(_._1)
      )
      .callToResult
  }

  private def errorsToResult(errors: NonEmptyChain[String]): CompilationResult = {
    CompilationResult.errs(errors.toChain.toList)
  }

  private def findBySuffix(
    compiled: Seq[Generated],
    suffix: String,
    errorMsg: String
  ): ValidatedNec[String, String] = {
    Validated
      .fromOption(
        compiled.find(_.suffix == suffix).map(_.content), {
          logger.error(errorMsg)
          NonEmptyChain.one(errorMsg)
        }
      )
  }

  extension (res: Chain[Validated[NonEmptyChain[String], GeneratedSource]])
      def toSourcesResult: ValidatedNec[String, CompilationResult] =
        res.sequence.map(_.toList.toJSArray).map(sources => CompilationResult.result(sources = sources))

  private def compiledToTsSourceResult(
    compiled: Chain[AquaCompiled[FileModuleId]]
  ): ValidatedNec[String, CompilationResult] = {
    compiled.map { c =>
      findBySuffix(
        c.compiled,
        TypeScriptBackend.ext,
        "Unreachable. TypeScript backend must generate content."
      )
        .map(GeneratedSource.tsSource(c.sourceId.toString, _))
    }.toSourcesResult
  }

  private def compiledToJsSourceResult(
    compiled: Chain[AquaCompiled[FileModuleId]]
  ): ValidatedNec[String, CompilationResult] = {
    compiled.map { c =>
      findBySuffix(
        c.compiled,
        JavaScriptBackend.dtsExt,
        "Unreachable. JavaScript backend must generate '.d.ts' content."
      ).andThen { tsTypes =>
        findBySuffix(
          c.compiled,
          JavaScriptBackend.ext,
          "Unreachable. JavaScript backend must generate '.js' content."
        ).map(jsSource => GeneratedSource.jsSource(c.sourceId.toString, jsSource, tsTypes))
      }

    }.toSourcesResult
  }

  private def generatedToAirResult(
    compiled: Chain[AquaCompiled[FileModuleId]]
  ): CompilationResult = {
    val generated = compiled.toList.flatMap(_.compiled)
    val serviceDefs = generated.flatMap(_.services).map(s => s.name -> ServiceDefJs(s))
    val functions = generated.flatMap(
      _.air.map(as => (as.name, AquaFunction(FunctionDefJs(as.funcDef), as.air)))
    )

    CompilationResult.result(
      js.Dictionary.apply(serviceDefs: _*),
      js.Dictionary.apply(functions: _*)
    )

  }
}
