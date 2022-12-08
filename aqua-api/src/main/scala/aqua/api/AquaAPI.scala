package aqua.api

import aqua.backend.{AirString, Backend, Generated}
import aqua.compiler.*
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.logging.LogLevels
import aqua.constants.Constants
import aqua.io.*
import aqua.parser.lexer.{LiteralToken, Token}
import aqua.parser.lift.FileSpan.F
import aqua.parser.lift.{FileSpan, Span}
import aqua.parser.{ArrowReturnError, BlockIndentError, LexerError, ParserError}
import aqua.semantics.{CompilerState, HeaderError, RulesViolated, WrongAST}
import aqua.{AquaIO, SpanParser}
import aqua.model.transform.{Transform, TransformConfig}
import aqua.backend.api.APIBackend
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.data.Validated.{Invalid, Valid, invalidNec, validNec}
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
import scala.scalajs.js.{UndefOr, undefined}
import aqua.js.{FunctionDefJs, ServiceDefJs}
import aqua.model.AquaContext
import aqua.res.AquaRes
import cats.Applicative

@JSExportAll
case class AquaFunction(funcDef: FunctionDefJs, script: String)

@JSExportAll
case class AquaConfig(logLevel: String = "info", constants: js.Map[String, String] = js.Map.empty, noXor: Boolean = false, noRelay: Boolean = false)

@JSExportAll
case class CompilationResult(
  services: js.Array[ServiceDefJs],
  functions: js.Map[String, AquaFunction]
)

@JSExportTopLevel("Aqua")
object AquaAPI extends App with Logging {

  @JSExport
  def compile(
    pathStr: String,
    imports: scalajs.js.Array[String],
    aquaConfig: AquaConfig
  ): scalajs.js.Promise[CompilationResult] = {

    LogLevels.fromString(aquaConfig.logLevel) match {
      case Valid(level) => js.Promise.resolve("")
      case Invalid(error) => js.Promise.reject(error)
    }

    println("SCALA")

    import aqua.ErrorRendering.showError

    scribe.Logger.root
      .withMinimumLevel(scribe.Level.Trace)
      .replace()

    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]

    val path = Path(pathStr)
    val sources = new AquaFileSources[IO](path, imports.toList.map(Path.apply))
    val config = AquaCompilerConf()
    val transformConfig = TransformConfig()

    val proc = for {

      res <- CompilerAPI
        .compile[IO, AquaFileError, FileModuleId, FileSpan.F](
          sources,
          SpanParser.parser,
          new AirValidator[IO] {
            override def init(): IO[Unit] = Applicative[IO].pure(())
            override def validate(airs: List[AirString]): IO[ValidatedNec[String, Unit]] =
              Applicative[IO].pure(validNec(()))
          },
          new Backend.Transform:
            override def transform(ex: AquaContext): AquaRes =
              println("AQUA CONTEXT: " + ex)
              Transform.contextRes(ex, transformConfig)

            override def generate(aqua: AquaRes): Seq[Generated] = APIBackend.generate(aqua)
          ,
          config
        )
      jsResult <- res match {
        case Valid(compiled) =>
          val allGenerated: List[Generated] = compiled.toList.flatMap(_.compiled)
          val serviceDefs = allGenerated.flatMap(_.services).map(s => ServiceDefJs(s)).toJSArray
          val functions = allGenerated.flatMap(_.air.map(as => (as.name, AquaFunction(FunctionDefJs(as.funcDef), as.air))))

          IO.pure(CompilationResult(serviceDefs, js.Map.apply(functions:_*)))
        case Invalid(errChain) =>
          errChain.map(_.show).distinct.map(OutputPrinter.errorF[IO]).sequence
          IO.raiseError[CompilationResult](new Error("Compilation failed."))
      }
    } yield {
      jsResult
    }

    proc.unsafeToFuture().toJSPromise
  }
}
