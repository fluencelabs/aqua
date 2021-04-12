package aqua.linker.io

import cats.data.EitherT

import java.nio.file.Path
import cats.effect.Sync

case class FileModuleId(file: Path)

object FileModuleId {

  private def findFirstF[F[_]: Sync](
    in: LazyList[Path],
    notFound: EitherT[F, LinkerError, FileModuleId]
  ): EitherT[F, LinkerError, FileModuleId] =
    in.headOption.fold(notFound)(p =>
      EitherT
        .liftAttemptK[F, Throwable]
        .apply(
          Sync[F].suspend(Sync.Type.Blocking)(p.toFile.isFile)
        )
        .leftMap[LinkerError](FileSystemError)
        .recover(_ => false)
        .flatMap {
          case true =>
            EitherT
              .liftAttemptK[F, Throwable]
              .apply(
                Sync[F].suspend(Sync.Type.Blocking)(FileModuleId(p.toAbsolutePath))
              )
              .leftMap[LinkerError](FileSystemError)
          case false => findFirstF(in.tail, notFound)
        }
    )

  def resolve[F[_]: Sync](
    src: Path,
    imports: LazyList[Path]
  ): EitherT[F, LinkerError, FileModuleId] =
    findFirstF(
      imports
        .map(_.resolve(src)),
      EitherT.leftT(FileNotFound(src, imports))
    )
}
