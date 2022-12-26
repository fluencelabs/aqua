package aqua.run

import aqua.ErrorRendering.showError
import aqua.compiler.{AquaCompiler, AquaCompilerConf, CompilerAPI}
import aqua.files.{AquaFileSources, FileModuleId}
import aqua.{AquaIO, SpanParser}
import aqua.io.{AquaFileError, AquaPath, PackagePath, Prelude}
import aqua.model.transform.TransformConfig
import aqua.model.{AquaContext, FuncArrow}
import aqua.parser.lift.FileSpan
import aqua.run.CliFunc
import cats.data.Validated.{invalidNec, validNec}
import cats.data.{Chain, NonEmptyList, Validated, ValidatedNec}
import cats.effect.IO
import cats.effect.kernel.{Async, Clock}
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monad.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import fs2.io.file.{Files, Path}
import scribe.Logging

import scala.concurrent.duration.Duration

class FuncCompiler[F[_]: Files: AquaIO: Async](
  input: Option[AquaPath],
  imports: List[Path],
  transformConfig: TransformConfig,
  withRunImport: Boolean = false
) extends Logging {

  private def compileToContext(
    path: Path,
    imports: List[Path],
    config: AquaCompilerConf = AquaCompilerConf(transformConfig.constantsList)
  ) = {
    val sources = new AquaFileSources[F](path, imports)
    CompilerAPI
      .compileToContext[F, AquaFileError, FileModuleId, FileSpan.F](
        sources,
        SpanParser.parser,
        config
      )
      .map(_.leftMap(_.map(_.show)))
  }

  private def compileBuiltins() = {
    for {
      path <- PackagePath.builtin.getPath()
      context <- compileToContext(path, Nil)
    } yield {
      context
    }
  }

  // Compile and get only one function
  def compile(
    withPrelude: Boolean = true,
    withBuiltins: Boolean = false
  ): F[ValidatedNec[String, Chain[AquaContext]]] = {
    for {
      importPaths <- if (withPrelude) Prelude.init[F](withRunImport).map(_.importPaths) else Nil.pure[F]
      // compile builtins and add it to context
      builtinsV <-
        if (withBuiltins) compileBuiltins()
        else validNec[String, Chain[AquaContext]](Chain.empty).pure[F]
      compileResult <- input.map { ap =>
        // compile only context to wrap and call function later
        Clock[F].timed(
          ap.getPath().flatMap(p => compileToContext(p, importPaths ++ imports))
        )
      }.getOrElse((Duration.Zero, validNec[String, Chain[AquaContext]](Chain.empty)).pure[F])
      (compileTime, contextV) = compileResult
    } yield {
      logger.debug(s"Compile time: ${compileTime.toMillis}ms")
      // add builtins to the end of context
      contextV.andThen(c => builtinsV.map(bc => c ++ bc))
    }
  }
}

object FuncCompiler {

  def findFunction(
    contexts: Chain[AquaContext],
    func: CliFunc
  ): ValidatedNec[String, FuncArrow] =
    func.ability
      .fold(
        contexts
          .collectFirstSome(_.allFuncs.get(func.name))
      )(ab => contexts.collectFirstSome(_.abilities.get(ab).flatMap(_.allFuncs.get(func.name))))
      .map(validNec)
      .getOrElse(
        Validated.invalidNec[String, FuncArrow](
          s"There is no function '${func.ability.map(_ + ".").getOrElse("")}${func.name}' or it is not exported. Check the spelling or see https://fluence.dev/docs/aqua-book/language/header/#export"
        )
      )
}
