package aqua

import aqua.backend.Backend
import aqua.compiler.{AquaCompiler, AquaError}
import aqua.files.{AquaFileSources, FileModuleId}
import aqua.io.*
import aqua.model.result.transform.GenerationConfig
import aqua.parser.lift.FileSpan
import cats.data.*
import cats.syntax.functor.*
import cats.syntax.show.*
import cats.{Monad, Show}
import scribe.Logging
import fs2.io.file.{Files, Path}

object AquaPathCompiler extends Logging {

  def compileFilesTo[F[_]: AquaIO: Monad: Files](
    srcPath: Path,
    imports: List[Path],
    targetPath: Path,
    backend: Backend,
    bodyConfig: GenerationConfig
  ): F[ValidatedNec[String, Chain[String]]] = {
    import ErrorRendering.showError
    val sources = new AquaFileSources[F](srcPath, imports)
    AquaCompiler
      .compileTo[F, AquaFileError, FileModuleId, FileSpan.F, String](
        sources,
        (fmid, src) => FileSpan.fileSpanLiftParser(fmid.file.toString, src),
        backend,
        bodyConfig,
        sources.write(targetPath)
      )
      .map(_.leftMap(_.map(_.show)))
  }

}
