package aqua.compiler.io

import aqua.parser.lift.FileSpan
import cats.data.EitherT
import cats.effect.kernel.Concurrent
import cats.syntax.applicative._

import java.nio.file.Path

case class FileModuleId(file: Path)

object FileModuleId {

  private def findFirstF[F[_]: Concurrent](
    in: List[Path],
    notFound: EitherT[F, AquaFileError, FileModuleId]
  ): EitherT[F, AquaFileError, FileModuleId] =
    in.headOption.fold(notFound)(p =>
      EitherT(
        Concurrent[F].attempt(p.toFile.isFile.pure[F])
      )
        .leftMap[AquaFileError](FileSystemError)
        .recover({ case _ => false })
        .flatMap {
          case true =>
            EitherT(
              Concurrent[F].attempt(FileModuleId(p.toAbsolutePath.normalize()).pure[F])
            ).leftMap[AquaFileError](FileSystemError)
          case false =>
            findFirstF(in.tail, notFound)
        }
    )

  /**
   * Checks if a file existed in the list of possible paths
   */
  def resolve[F[_]: Concurrent](
    focus: FileSpan.Focus,
    src: Path,
    imports: List[Path]
  ): EitherT[F, AquaFileError, FileModuleId] =
    findFirstF(
      imports
        .map(_.resolve(src)),
      EitherT.leftT(FileNotFound(focus, src, imports))
    )
}
