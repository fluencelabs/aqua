package aqua.compiler

import aqua.compiler.io.AquaFileError
import aqua.parser.lift.FileSpan
import cats.data.{Chain, EitherT, ValidatedNec}

import java.nio.file.Path

trait AquaIO[F[_]] {
  def readFile(file: Path): EitherT[F, AquaFileError, String]

  def resolve(
    focus: FileSpan.Focus,
    src: Path,
    imports: List[Path]
  ): EitherT[F, AquaFileError, Path]

  def listAqua(folder: Path): F[ValidatedNec[AquaFileError, Chain[Path]]]

  def writeFile(file: Path, content: String): EitherT[F, AquaFileError, Unit]
}

object AquaIO {
  def apply[F[_]](implicit aio: AquaIO[F]): AquaIO[F] = aio
}
