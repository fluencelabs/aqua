package aqua

import aqua.backend.Backend
import aqua.compiler.{AquaCompiled, AquaCompiler, AquaError}
import aqua.files.{AquaFileSources, FileModuleId}
import aqua.io.*
import aqua.model.transform.TransformConfig
import aqua.parser.{Ast, LexerError}
import aqua.parser.lift.FileSpan
import cats.data.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.show.*
import cats.{~>, Applicative, Eval, Monad, Show}
import scribe.Logging
import fs2.io.file.{Files, Path}
import aqua.parser.lift.{LiftParser, Span}
import cats.parse.LocationMap
import cats.~>
import aqua.parser.lift.LiftParser.LiftErrorOps
import Span.spanLiftParser
import aqua.parser.Parser

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
    val sources = new AquaFileSources[F](srcPath, imports)
    AquaCompiler
      .compileTo[F, AquaFileError, FileModuleId, FileSpan.F, String](
        sources,
        SpanParser.parser,
        backend,
        transformConfig,
        targetPath.map(sources.write).getOrElse(dry[F])
      )
      .map(_.leftMap(_.map(_.show)))
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
