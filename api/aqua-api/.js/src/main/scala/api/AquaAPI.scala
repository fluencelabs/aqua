package api

import api.types.{AquaConfig, AquaFunction, CompilationResult, Input}
import aqua.ErrorRendering.showError
import aqua.raw.value.ValueRaw
import aqua.api.{APICompilation, AquaAPIConfig}
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
          APICompilation
            .compileString(
              i.input,
              importsList,
              config
            )
            .flatMap(_.map(generatedToCompilationResult).leftMap(generatedToCompilationErrors).merge)
        case p: types.Path =>
          APICompilation
            .compilePath(
              p.path,
              importsList,
              config
            )
            .flatMap(_.map(generatedToCompilationResult).leftMap(generatedToCompilationErrors).merge)
        case c: types.Call =>
          val path = c.input match {
            case i: types.Input => i.input
            case p: types.Path => p.path
          }
          APICompilation
            .compileCall(
              c.functionCall,
              path,
              importsList,
              config,
              vr => VarJson.checkDataGetServices(vr, Some(c.arguments)).map(_._1)
            )
            .map {
              case Valid((definitions, air)) =>
                CompilationResult.result(
                  js.Dictionary(),
                  js.Dictionary(),
                  Some(AquaFunction(FunctionDefJs(definitions), air)),
                  None
                )
              case Invalid(err) => CompilationResult.errs(err.toChain.toList)
            }
      }
    } match {
      case Valid(v) => v.unsafeToFuture().toJSPromise
      case Invalid(errs) => js.Promise.resolve(CompilationResult.errs(errs.toChain.toList))
    }

  }

  private def generatedToCompilationErrors(errors: NonEmptyChain[String]): IO[CompilationResult] = {
    IO.pure(CompilationResult.errs(errors.toChain.toList))
  }

  private def generatedToCompilationResult(generated: List[Generated]): IO[CompilationResult] = {
    val serviceDefs = generated.flatMap(_.services).map(s => s.name -> ServiceDefJs(s))
    val functions = generated.flatMap(
      _.air.map(as => (as.name, AquaFunction(FunctionDefJs(as.funcDef), as.air)))
    )

    IO.pure(
      CompilationResult.result(
        js.Dictionary.apply(serviceDefs: _*),
        js.Dictionary.apply(functions: _*),
        None,
        None
      )
    )
  }
}
