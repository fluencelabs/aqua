package aqua.run

import aqua.compiler.{AquaCompilerConf, CompileResult, CompilerAPI}
import aqua.files.{AquaFileSources, FileModuleId, Imports}
import aqua.io.{AquaFileError, AquaPath, PackagePath}
import aqua.model.transform.TransformConfig
import aqua.model.{AquaContext, FuncArrow}
import aqua.parser.lift.FileSpan
import aqua.{AquaIO, SpanParser}

import cats.data.{Chain, ValidatedNec}
import cats.effect.kernel.{Async, Clock}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import fs2.io.file.{Files, Path}
import scribe.Logging

class FuncCompiler[F[_]: Files: AquaIO: Async](
  input: Option[AquaPath],
  imports: Imports,
  transformConfig: TransformConfig,
  config: AquaCompilerConf
) extends Logging {

  type Result = [A] =>> CompileResult[FileModuleId, AquaFileError, FileSpan.F][A]

  private def compileToContext(
    path: Path
  ): F[Result[Chain[AquaContext]]] = {
    val sources = new AquaFileSources[F](path, imports)
    CompilerAPI.compileToContext[F, AquaFileError, FileModuleId, FileSpan.F](
      sources,
      SpanParser.parser,
      config
    )
  }

  private def compileBuiltins(): F[Result[Chain[AquaContext]]] =
    for {
      path <- PackagePath.builtin.getPath()
      context <- compileToContext(path)
    } yield context

  // Compile and get only one function
  def compile(
    withBuiltins: Boolean = false
  ): F[Result[Chain[AquaContext]]] = {
    for {
      // compile builtins and add it to context
      builtinsV <-
        if (withBuiltins) compileBuiltins()
        else Chain.empty.pure[Result].pure[F]
      compileResult <- input.traverse { ap =>
        // compile only context to wrap and call function later
        Clock[F].timed(
          ap.getPath().flatMap(p => compileToContext(p))
        )
      }
      (compileTime, contextV) = compileResult.orEmpty
    } yield {
      logger.debug(s"Compile time: ${compileTime.toMillis}ms")
      // add builtins to the end of context
      (contextV, builtinsV).mapN(_ ++ _)
    }
  }
}

object FuncCompiler {

  def findFunction(
    contexts: Chain[AquaContext],
    func: CliFunc
  ): ValidatedNec[String, FuncArrow] = contexts
    .collectFirstSome(_.allFuncs.get(func.name))
    .toValidNec(
      s"There is no function '${func.name}' or it is not exported. Check the spelling or see https://fluence.dev/docs/aqua-book/language/header/#export"
    )
}
