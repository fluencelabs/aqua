package aqua.files

import aqua.compiler.AquaSources
import aqua.io.AquaFileError
import cats.data.{Chain, ValidatedNec}

class AquaFileSources[F[_]: AquaFilesIO] extends AquaSources[F, AquaFileError, FileModuleId] {
  private val filesIO = implicitly[AquaFilesIO[F]]

  override def sources: F[ValidatedNec[AquaFileError, Chain[(FileModuleId, String)]]] =
    ???

  override def resolve(
    from: FileModuleId,
    imp: String
  ): F[ValidatedNec[AquaFileError, FileModuleId]] = ???

  override def load(file: FileModuleId): F[ValidatedNec[AquaFileError, String]] = ???
}
