package aqua

import aqua.compiler.AquaCompiler
import aqua.ErrorRendering.showError
import aqua.files.{AquaFileSources, AquaFilesIO, FileModuleId}
import aqua.io.AquaFileError
import aqua.model.{AquaContext, FuncArrow}
import aqua.model.transform.TransformConfig
import aqua.parser.lift.FileSpan
import aqua.run.RunCommand.logger
import cats.data.{Chain, Validated, ValidatedNec}
import cats.data.Validated.{invalidNec, validNec}
import cats.effect.IO
import cats.effect.kernel.{Async, Clock}
import fs2.io.file.{Files, Path}
import cats.syntax.monad.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.show.*
import scribe.Logging

// Function compiler
class FuncCompiler[F[_]: Files: AquaIO: Async](
  input: Path,
  imports: List[Path],
  transformConfig: TransformConfig,
  withRunImport: Boolean = false
) extends Logging {

  private def findFunction(
    contexts: Chain[AquaContext],
    func: CliFunc
  ): ValidatedNec[String, FuncArrow] =
    contexts
      .collectFirstSome(_.allFuncs.get(func.name))
      .map(validNec)
      .getOrElse(
        Validated.invalidNec[String, FuncArrow](
          s"There is no function '${func.name}'. Check the spelling. Or there is no exported function '${func.name}'. See https://doc.fluence.dev/aqua-book/language/header#export on exporting functions"
        )
      )

  // Compile and get only one function
  def compile(func: CliFunc): F[ValidatedNec[String, FuncArrow]] = {
    implicit val aio: AquaIO[IO] = new AquaFilesIO[IO]

    for {
      prelude <- Prelude.init[F](withRunImport)
      sources = new AquaFileSources[F](input, prelude.importPaths ++ imports)
      // compile only context to wrap and call function later
      compileResult <- Clock[F].timed(
        AquaCompiler
          .compileToContext[F, AquaFileError, FileModuleId, FileSpan.F](
            sources,
            SpanParser.parser,
            transformConfig
          )
          .map(_.leftMap(_.map(_.show)))
      )
      (compileTime, contextV) = compileResult
    } yield {
      logger.debug(s"Compile time: ${compileTime.toMillis}ms")
      contextV.andThen(c => findFunction(c, func))
    }
  }
}
