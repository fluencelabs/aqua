package aqua.lsp

import aqua.compiler.*
import aqua.compiler.AquaError.SourcesError
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId, Imports}
import aqua.io.*
import aqua.parser.lift.FileSpan
import aqua.parser.lift.FileSpan.F
import aqua.raw.ConstantRaw
import aqua.{AquaIO, SpanParser}

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, Validated}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.option.*
import fs2.io.file.{Files, Path}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.*
import scribe.Logging

@JSExportTopLevel("AquaLSP")
object AquaLSP extends Logging {

  import ResultHelper.*

  type ImportsJS = js.Dictionary[
    js.Dictionary[js.Array[String]]
  ]

  @JSExport
  def compile(
    pathStr: String,
    imports: ImportsJS
  ): js.Promise[CompilationResult] = {
    logger.debug(s"Compiling '$pathStr' with imports: $imports")

    given AquaIO[IO] = new AquaFilesIO[IO]

    val path = Path(pathStr)
    val pathId = FileModuleId(path)
    val sources = new AquaFileSources[IO](
      path,
      importsToIO(imports)
    )
    val config = AquaCompilerConf(ConstantRaw.defaultConstants(None))

    val proc = for {
      res <- LSPCompiler.compileToLsp[IO, AquaFileError, FileModuleId, FileSpan.F](
        sources,
        SpanParser.parser,
        config
      )
    } yield {
      val fileRes = res.andThen(
        _.get(pathId).toValidNec(
          SourcesError(Unresolvable(s"Unexpected. No file $pathStr in compiler results"))
        )
      )

      logger.debug("Compilation done.")

      fileRes match {
        case Valid(lsp) =>
          lspToCompilationResult(lsp)
        case Invalid(e) =>
          val errors = e.toChain.toList.flatMap(errorToInfo)
          logger.debug("Errors: " + errors.mkString("\n"))
          CompilationResult(errors.toJSArray)
      }
    }

    proc.unsafeToFuture().toJSPromise
  }

  private def importsToIO(
    imports: ImportsJS
  ): Imports = Imports(
    imports.toMap.map { case (pathPrefix, settings) =>
      Path(pathPrefix) -> Imports.PathSettings(
        settings.toMap.map { case (importPrefix, locations) =>
          importPrefix -> locations.toList.map(Path.apply)
        }
      )
    }
  )
}
