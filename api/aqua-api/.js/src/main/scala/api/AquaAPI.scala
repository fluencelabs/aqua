package api

import api.types.{AquaConfig, AquaFunction, CompilationResult, GeneratedSource, Input}
import aqua.ErrorRendering.showError
import aqua.raw.value.ValueRaw
import aqua.api.{APICompilation, AirType, AquaAPIConfig, JavaScriptType, TypeScriptType}
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
import scala.scalajs.js.{Promise, UndefOr, undefined, |}
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

  def compileAll(
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
        _.map { compiled =>
          config.targetType match {
            case AirType => generatedToAirResult(compiled.toList.flatMap(_.compiled))
            case TypeScriptType =>
              val sources = compiled.map { c =>
                GeneratedSource.tsSource(c.sourceId.toString, c.compiled.head.content)
              }.toList.toJSArray
              CompilationResult.result(sources = sources)

            case JavaScriptType =>
              val sources = compiled.map { c =>
                val tsTypes = c.compiled.find(_.suffix == JavaScriptBackend.dtsExt).map(_.content).getOrElse("")
                val jsSource = c.compiled.find(_.suffix == JavaScriptBackend.ext).map(_.content).getOrElse("")
                GeneratedSource.jsSource(c.sourceId.toString, jsSource, tsTypes)
              }.toList.toJSArray
              CompilationResult.result(sources = sources)
          }
        }.leftMap(generatedToCompilationErrors).merge
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

  def compileCall(call: types.Call, imports: List[String], config: AquaAPIConfig) = {
    val path = call.input match {
      case i: types.Input => i.input
      case p: types.Path => p.path
    }

    extension (res: IO[ValidatedNec[String, (FunctionDef, String)]])
      def callToResult: IO[CompilationResult] = res.map(
        _.map { case (definitions, air) =>
          CompilationResult.result(call = Some(AquaFunction(FunctionDefJs(definitions), air)))
        }.leftMap(generatedToCompilationErrors).merge
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

  @JSExport
  def compile(
    input: types.Input | types.Path | types.Call,
    imports: js.Array[String],
    aquaConfigJS: js.UndefOr[AquaConfig]
  ): Promise[CompilationResult] = {
    (aquaConfigJS.toOption.map(cjs => AquaConfig.fromJS(cjs)) match {
      case Some(Valid(conf)) => validNec(conf)
      case Some(inv @ Invalid(_)) => inv
      case None => validNec(AquaAPIConfig())
    }).map { config =>
      val importsList = imports.toList

      input match {
        case i: types.Input =>
          compileAll(i, importsList, config)
        case p: types.Path =>
          compileAll(p, importsList, config)
        case c: types.Call =>
          compileCall(c, importsList, config)

      }
    } match {
      case Valid(v) => v.unsafeToFuture().toJSPromise
      case Invalid(errs) => js.Promise.resolve(CompilationResult.errs(errs.toChain.toList))
    }

  }

  private def generatedToCompilationErrors(errors: NonEmptyChain[String]): CompilationResult = {
    CompilationResult.errs(errors.toChain.toList)
  }

  private def generatedToAirResult(generated: List[Generated]): CompilationResult = {
    val serviceDefs = generated.flatMap(_.services).map(s => s.name -> ServiceDefJs(s))
    val functions = generated.flatMap(
      _.air.map(as => (as.name, AquaFunction(FunctionDefJs(as.funcDef), as.air)))
    )

    CompilationResult.result(
      js.Dictionary.apply(serviceDefs: _*),
      js.Dictionary.apply(functions: _*),
    )

  }
}
