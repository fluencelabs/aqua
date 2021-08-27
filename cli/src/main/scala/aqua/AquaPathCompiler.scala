package aqua

import aqua.backend.Backend
import aqua.compiler.{AquaCompiler, AquaError}
import aqua.files.{AquaFileSources, FileModuleId}
import aqua.io.*
import aqua.model.transform.TransformConfig
import aqua.parser.Ast.parser
import aqua.parser.lift.FileSpan
import cats.data.*
import cats.syntax.functor.*
import cats.syntax.show.*
import cats.{~>, Eval, Monad, Show}
import scribe.Logging
import fs2.io.file.{Files, Path}
import aqua.parser.lift.{LiftParser, Span}
import cats.parse.LocationMap

object AquaPathCompiler extends Logging {

  import Span.spanLiftParser
  lazy val spanParser = parser[Span.F]()

  def compileFilesTo[F[_]: AquaIO: Monad: Files](
    srcPath: Path,
    imports: List[Path],
    targetPath: Path,
    backend: Backend,
    bodyConfig: TransformConfig
  ): F[ValidatedNec[String, Chain[String]]] = {
    import ErrorRendering.showError
    val sources = new AquaFileSources[F](srcPath, imports)
    AquaCompiler
      .compileTo[F, AquaFileError, FileModuleId, Span.F, FileSpan.F, String](
        sources,
        (id, source) =>
          (span: Span.F[?]) =>
            (
              FileSpan(id.file.fileName.toString, Eval.later(LocationMap(source)), span._1),
              span._2
            ): FileSpan.F[?],
        spanParser,
        backend,
        bodyConfig,
        sources.write(targetPath)
      )
      .map(_.leftMap(_.map(_.show)))
  }

}
