package aqua.files

import aqua.AquaIO
import aqua.compiler.{AquaCompiled, AquaSources}
import aqua.io.{AquaFileError, FileSystemError}
import cats.Monad
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.implicits.catsSyntaxApplicativeId
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._

import java.nio.file.{Path, Paths}
import scala.util.Try

class AquaFileSources[F[_]: AquaIO: Monad](sourcesPath: Path, importFrom: List[Path])
    extends AquaSources[F, AquaFileError, FileModuleId] {
  private val filesIO = implicitly[AquaIO[F]]

  override def sources: F[ValidatedNec[AquaFileError, Chain[(FileModuleId, String)]]] =
    filesIO.listAqua(sourcesPath).flatMap {
      case Validated.Valid(files) =>
        files
          .map(f =>
            filesIO
              .readFile(f)
              .value
              .map[ValidatedNec[AquaFileError, Chain[(FileModuleId, String)]]] {
                case Left(err) => Validated.invalidNec(err)
                case Right(content) => Validated.validNec(Chain.one(FileModuleId(f) -> content))
              }
          )
          .traverse(identity)
          .map(
            _.foldLeft[ValidatedNec[AquaFileError, Chain[(FileModuleId, String)]]](
              Validated.validNec(Chain.nil)
            )(_ combine _)
          )
      case Validated.Invalid(e) =>
        Validated.invalidNec[AquaFileError, Chain[(FileModuleId, String)]](e.head).pure[F]
    }

  override def resolve(
    from: FileModuleId,
    imp: String
  ): F[ValidatedNec[AquaFileError, FileModuleId]] = {
    Validated.fromEither(Try(Paths.get(imp)).toEither.leftMap(FileSystemError)) match {
      case Validated.Valid(importP) =>
        filesIO
          .resolve(from.file, importP +: importFrom)
          .bimap(NonEmptyChain.one, FileModuleId(_))
          .value
          .map(Validated.fromEither)
      case Validated.Invalid(err) => Validated.invalidNec[AquaFileError, FileModuleId](err).pure[F]
    }

  }

  override def load(file: FileModuleId): F[ValidatedNec[AquaFileError, String]] =
    filesIO.readFile(file.file).leftMap(NonEmptyChain.one).value.map(Validated.fromEither)

  def write(
    targetPath: Path
  )(ac: AquaCompiled[FileModuleId]): F[Validated[AquaFileError, String]] = {
    // TODO: this does not respect source subfolders
    val target = targetPath.resolve(
      ac.sourceId.file.getFileName.toString.stripSuffix(".aqua") + ac.compiled.suffix
    )

    filesIO
      .writeFile(
        target,
        ac.compiled.content
      )
      .as(target.toString)
      .value
      .map(Validated.fromEither)
  }
}
