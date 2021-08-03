package aqua

import aqua.backend.Backend
import aqua.compiler.{AquaCompiler, AquaError}
import aqua.files.{AquaFileSources, FileModuleId}
import aqua.io._
import aqua.model.transform.GenerationConfig
import aqua.parser.lift.FileSpan
import cats.data._
import cats.syntax.functor._
import cats.{Monad, Show}
import wvlet.log.LogSupport

import java.nio.file.Path

object AquaPathCompiler extends LogSupport {

  def compileFilesTo[F[_]: AquaIO: Monad](
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
      .map(_.leftMap(_.map { err: AquaError[FileModuleId, AquaFileError, FileSpan.F] =>
        Show[AquaError[FileModuleId, AquaFileError, FileSpan.F]].show(err)
      }))
  }

}
