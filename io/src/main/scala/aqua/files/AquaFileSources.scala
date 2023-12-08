package aqua.files

import aqua.AquaIO
import aqua.compiler.{AquaCompiled, AquaSources}
import aqua.io.{AquaFileError, FileSystemError, ListAquaErrors}
import aqua.syntax.eithert.*

import cats.data.EitherT
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.implicits.catsSyntaxApplicativeId
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.monad.*
import cats.syntax.traverse.*
import cats.syntax.validated.*
import cats.{Functor, Monad}
import fs2.io.file.{Files, Path}
import scala.util.Try
import scribe.Logging

/**
 * Trait that implements `read` and `resolveImport` methods for `AquaSources` trait.
 * Imports are resolved with `imports` map.
 */
trait AquaFileImports[F[_]: Functor: AquaIO] extends AquaSources[F, AquaFileError, FileModuleId] {

  /**
   * Map (path prefix -> list of import paths for this prefix)
   */
  def imports: Map[Path, List[Path]]

  override def resolveImport(
    from: FileModuleId,
    imported: String
  ): F[ValidatedNec[AquaFileError, FileModuleId]] =
    AquaIO[F]
      .resolve(
        Path(imported),
        gatherImportsFor(from)
      )
      .map(FileModuleId.apply)
      .toValidatedNec

  override def load(file: FileModuleId): F[ValidatedNec[AquaFileError, String]] =
    AquaIO[F].readFile(file.file).toValidatedNec

  /**
   * Gather imports locations for given file id
   * by matching its path with prefixes in `imports` map.
   * Longer prefixes are prioritized.
   */
  private def gatherImportsFor(id: FileModuleId): List[Path] = {
    val idNorm = id.file.normalize.absolute
    val matchedImports = imports.toList.map { case (prefix, paths) =>
      prefix.normalize.absolute -> paths
    }.filter { case (prefix, _) =>
      idNorm.startsWith(prefix)
    }.sortBy { case (prefix, _) =>
      prefix.toString.length
    }.reverse.flatMap { case (_, paths) => paths }

    // TODO: Check if `idNorm` is a dir already?
    val idDir = idNorm.parent

    matchedImports.prependedAll(idDir)
  }
}

/**
 * Aqua sources that are read from file system.
 */
class AquaFileSources[F[_]: Monad: AquaIO](
  sourcesPath: Path,
  override val imports: Map[Path, List[Path]]
) extends AquaFileImports[F] with Logging {

  override def sources: F[ValidatedNec[AquaFileError, Chain[(FileModuleId, String)]]] =
    (for {
      files <- EitherT.fromValidatedF(
        AquaIO[F].listAqua(sourcesPath)
      )
      contents <- EitherT.fromValidatedF(
        files
          .traverse(file =>
            AquaIO[F]
              .readFile(file)
              .map(content => FileModuleId(file) -> content)
              .toValidatedNec
          )
          .map(_.sequence)
      )
    } yield contents).toValidated
}

/**
 * Aqua sources that are read from string map.
 */
class AquaStringSources[F[_]: Monad: AquaIO](
  sourcesMap: Map[FileModuleId, String],
  override val imports: Map[Path, List[Path]]
) extends AquaFileImports[F] {

  override def sources: F[ValidatedNec[AquaFileError, Chain[(FileModuleId, String)]]] =
    Chain.fromSeq(sourcesMap.toSeq).validNec.pure[F]
}
