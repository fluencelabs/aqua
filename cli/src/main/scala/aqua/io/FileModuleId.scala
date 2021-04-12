package aqua.io

import aqua.parser.lift.FileSpan
import cats.data.EitherT
import cats.effect.Sync

import java.nio.file.Path

case class FileModuleId(file: Path)

object FileModuleId {

  private def findFirstF[F[_]: Sync](
    in: LazyList[Path],
    notFound: EitherT[F, AquaFileError, FileModuleId]
  ): EitherT[F, AquaFileError, FileModuleId] =
    in.headOption.fold(notFound)(p =>
      EitherT
        .liftAttemptK[F, Throwable]
        .apply(
          Sync[F].suspend(Sync.Type.Blocking)(p.toFile.isFile)
        )
        .leftMap[AquaFileError](FileSystemError)
        .recover(_ => false)
        .flatMap {
          case true =>
            EitherT
              .liftAttemptK[F, Throwable]
              .apply(
                Sync[F].suspend(Sync.Type.Blocking)(FileModuleId(p.toAbsolutePath))
              )
              .leftMap[AquaFileError](FileSystemError)
          case false => findFirstF(in.tail, notFound)
        }
    )

  def resolve[F[_]: Sync](
    focus: FileSpan.Focus,
    src: Path,
    imports: LazyList[Path]
  ): EitherT[F, AquaFileError, FileModuleId] =
    findFirstF(
      imports
        .map(_.resolve(src)),
      EitherT.leftT(FileNotFound(focus, src, imports))
    )
}
