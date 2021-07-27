package aqua

import aqua.backend.Backend
import aqua.compiler.AquaCompiler
import aqua.files.{AquaFileSources, FileModuleId}
import aqua.io._
import aqua.model.transform.BodyConfig
import aqua.parser.lift.FileSpan
import cats.data._
import cats.Monad
import wvlet.log.LogSupport
import cats.syntax.functor._

import java.nio.file.Path

object AquaPathCompiler extends LogSupport {

  def compileFilesTo[F[_]: AquaIO: Monad](
    srcPath: Path,
    imports: List[Path],
    targetPath: Path,
    backend: Backend,
    bodyConfig: BodyConfig
  ): F[ValidatedNec[String, Chain[String]]] = {
    val sources = new AquaFileSources[F](srcPath, imports)
    AquaCompiler
      .compileTo[F, AquaFileError, FileModuleId, FileSpan.F, String](
        sources,
        (fmid, src) => FileSpan.fileSpanLiftParser(fmid.file.toString, src),
        backend,
        bodyConfig,
        sources.write(targetPath)
      )
      .map(_.leftMap(_.map { case err =>
        // TODO: render errors properly
        err.toString
      }))
  }

}
