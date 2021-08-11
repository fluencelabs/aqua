package aqua

import aqua.io.AquaFileError
import cats.data.{Chain, EitherT, ValidatedNec}

import fs2.io.file.Path

trait AquaIO[F[_]] {
  def readFile(file: Path): EitherT[F, AquaFileError, String]

  def resolve(
    src: Path,
    imports: List[Path]
  ): EitherT[F, AquaFileError, Path]

  def listAqua(folder: Path): F[ValidatedNec[AquaFileError, Chain[Path]]]

  def writeFile(file: Path, content: String): EitherT[F, AquaFileError, Unit]
}

object AquaIO {
  def apply[F[_]](implicit aio: AquaIO[F]): AquaIO[F] = aio
}
