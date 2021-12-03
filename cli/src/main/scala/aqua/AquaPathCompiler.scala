package aqua

import aqua.backend.Backend
import aqua.compiler.{AquaCompiled, AquaCompiler, AquaError}
import aqua.files.{AquaFileSources, FileModuleId}
import aqua.io.*
import aqua.model.transform.TransformConfig
import aqua.parser.lift.LiftParser.LiftErrorOps
import aqua.parser.lift.Span.spanLiftParser
import aqua.parser.lift.{FileSpan, LiftParser, Span}
import aqua.parser.{Ast, LexerError, Parser}
import cats.data.*
import cats.parse.LocationMap
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.show.*
import cats.{~>, Applicative, Eval, Monad, Show}
import fs2.io.file.{Files, Path}
import scribe.Logging

object AquaPathCompiler extends Logging {

  /**
   * @param srcPath path to aqua sources
   * @param imports additional paths to possible aqua imports
   * @param targetPath path where compiled files will be created. Creates no output if empty
   * @param backend creates output (TS, JS, ...) from a model
   * @param transformConfig transformation configuration for a model
   * @return errors or result messages
   */
  def compileFilesTo[F[_]: AquaIO: Monad: Files](
    srcPath: Path,
    imports: List[Path],
    targetPath: Option[Path],
    backend: Backend,
    transformConfig: TransformConfig
  ): F[ValidatedNec[String, Chain[String]]] = {
    import ErrorRendering.showError
    (for {
      prelude <- Prelude.init(withPrelude = false)
      sources = new AquaFileSources[F](srcPath, imports ++ prelude.importPaths)
      compiler <- AquaCompiler
        .compileTo[F, AquaFileError, FileModuleId, FileSpan.F, String](
          sources,
          SpanParser.parser,
          backend,
          transformConfig,
          targetPath.map(sources.write).getOrElse(dry[F])
        )
    } yield {
      compiler
      // 'distinct' to delete all duplicated errors
    }).map(_.leftMap(_.map(_.show).distinct))

  }

  def dry[F[_]: Applicative](
    ac: AquaCompiled[FileModuleId]
  ): F[Seq[Validated[AquaFileError, String]]] =
    Seq(
      Validated.valid[AquaFileError, String](
        s"Source ${ac.sourceId.file}: compilation OK"
      )
    ).pure[F]

}
