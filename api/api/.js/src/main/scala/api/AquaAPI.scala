package api

import aqua.Rendering.given
import aqua.SpanParser
import aqua.api.TargetType.*
import aqua.api.{APICompilation, APIResult, AquaAPIConfig, Imports}
import aqua.backend.air.AirBackend
import aqua.backend.api.APIBackend
import aqua.backend.js.JavaScriptBackend
import aqua.backend.ts.TypeScriptBackend
import aqua.backend.{AirFunction, Backend, Generated}
import aqua.compiler.*
import aqua.constants.Constants
import aqua.definitions.FunctionDef
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.io.*
import aqua.js.{FunctionDefJs, ServiceDefJs, VarJson}
import aqua.logging.{LogFormatter, LogLevels}
import aqua.model.AquaContext
import aqua.parser.lexer.{LiteralToken, Token}
import aqua.parser.{ArrowReturnError, BlockIndentError, LexerError, ParserError}
import aqua.raw.ops.Call
import aqua.raw.ops.CallArrowRawTag
import aqua.raw.value.ValueRaw
import aqua.raw.value.{LiteralRaw, VarRaw}
import aqua.res.AquaRes

import api.types.{AquaConfig, AquaFunction, CompilationResult, GeneratedSource, Input}
import cats.Applicative
import cats.data.Validated.{Invalid, Valid, invalidNec, validNec}
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import fs2.io.file.{Files, Path}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.*
import scala.scalajs.js.{Promise, UndefOr, undefined, |}
import scribe.Logging

@JSExportTopLevel("Aqua")
object AquaAPI extends App with Logging {

  // See api-npm package for description of imports config
  type ImportsJS = js.Dictionary[
    js.Dictionary[js.Array[String]]
  ]

  /**
   * All-in-one function that support different inputs and backends
   * @param input can be a path to aqua file, string with a code or a function call
   * @param imports imports configuration
   * @param aquaConfigJS compiler config
   * @return compiler results depending on input and config
   */
  @JSExport
  def compile(
    input: types.Input | types.Path | types.Call,
    imports: ImportsJS,
    aquaConfigJS: js.UndefOr[AquaConfig]
  ): Promise[CompilationResult] = {
    aquaConfigJS.toOption
      .map(AquaConfig.fromJS)
      .getOrElse(validNec(AquaAPIConfig()))
      .traverse { config =>
        val apiImports = Imports.fromMap(
          imports.view
            .mapValues(
              _.toMap.view
                .mapValues(_.toList)
                .toMap
            )
            .toMap
        )

        input match {
          case i: (types.Input | types.Path) =>
            compileAll(i, apiImports, config)
          case c: types.Call =>
            compileCall(c, apiImports, config)

        }
      }
      .map(_.leftMap(errs => CompilationResult.errs(errs.toChain.toList)).merge)
      .unsafeToFuture()
      .toJSPromise
  }

  // Compile all non-call inputs
  private def compileAll(
    input: types.Input | types.Path,
    imports: Imports,
    config: AquaAPIConfig
  ): IO[CompilationResult] = {
    val backend: Backend = config.targetType match {
      case AirType => APIBackend
      case TypeScriptType => TypeScriptBackend()
      case JavaScriptType => JavaScriptBackend()
    }

    extension (res: APIResult[Chain[AquaCompiled[FileModuleId]]])
      def toResult: CompilationResult = {
        val (warnings, result) = res.value.run

        result.map { compiled =>
          (config.targetType match {
            case AirType => generatedToAirResult
            case TypeScriptType => compiledToTsSourceResult
            case JavaScriptType => compiledToJsSourceResult
          }).apply(compiled, warnings)
        }.leftMap(errorsToResult(_, warnings)).merge
      }

    input match {
      case i: types.Input =>
        APICompilation
          .compileString(
            i.input,
            imports,
            config,
            backend
          )
          .map(_.toResult)
      case p: types.Path =>
        APICompilation
          .compilePath(
            p.path,
            imports,
            config,
            backend
          )
          .map(_.toResult)
    }

  }

  // Compile a function call
  private def compileCall(
    call: types.Call,
    imports: Imports,
    config: AquaAPIConfig
  ): IO[CompilationResult] = {
    val path = call.input match {
      case i: types.Input => i.input
      case p: types.Path => p.path
    }

    extension (res: APIResult[(FunctionDef, String)])
      def callToResult: CompilationResult = {
        val (warnings, result) = res.value.run

        result.map { case (definitions, air) =>
          CompilationResult.result(
            call = Some(AquaFunction(FunctionDefJs(definitions), air)),
            warnings = warnings.toList
          )
        }.leftMap(errorsToResult(_, warnings)).merge
      }

    APICompilation
      .compileCall(
        call.functionCall,
        path,
        imports,
        config,
        vr => VarJson.checkDataGetServices(vr, Some(call.arguments)).map(_._1)
      )
      .map(_.callToResult)
  }

  private def errorsToResult(
    errors: NonEmptyChain[String],
    warnings: Chain[String]
  ): CompilationResult = CompilationResult.errs(
    errors.toChain.toList,
    warnings.toList
  )

  private def compiledToTsSourceResult(
    compiled: Chain[AquaCompiled[FileModuleId]],
    warnings: Chain[String]
  ): CompilationResult = CompilationResult.result(
    sources = compiled.toList
      .flatMap(c =>
        c.compiled
          .find(_.suffix == TypeScriptBackend.ext)
          .map(_.content)
          .map(GeneratedSource.tsSource(c.sourceId.toString, _))
      ),
    warnings = warnings.toList
  )

  private def compiledToJsSourceResult(
    compiled: Chain[AquaCompiled[FileModuleId]],
    warnings: Chain[String]
  ): CompilationResult = CompilationResult.result(
    sources = compiled.toList.flatMap { c =>
      for {
        dtsContent <- c.compiled
          .find(_.suffix == JavaScriptBackend.dtsExt)
          .map(_.content)
        jsContent <- c.compiled
          .find(_.suffix == JavaScriptBackend.ext)
          .map(_.content)
      } yield GeneratedSource.jsSource(c.sourceId.toString, jsContent, dtsContent)
    },
    warnings = warnings.toList
  )

  private def generatedToAirResult(
    compiled: Chain[AquaCompiled[FileModuleId]],
    warnings: Chain[String]
  ): CompilationResult = {
    val generated = compiled.toList.flatMap(_.compiled)
    val serviceDefs = generated.flatMap(_.services).map(s => s.name -> ServiceDefJs(s))
    val functions = generated.flatMap(
      _.air.map(as => as.name -> AquaFunction(FunctionDefJs(as.funcDef), as.air))
    )

    CompilationResult.result(
      services = serviceDefs.toMap,
      functions = functions.toMap,
      warnings = warnings.toList
    )

  }
}
