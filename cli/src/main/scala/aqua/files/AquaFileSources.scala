package aqua.files

import aqua.AquaIO
import aqua.compiler.{AquaCompiled, AquaSources}
import aqua.io.{AquaFileError, FileSystemError, ListAquaErrors}
import cats.{Functor, Monad}
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.implicits.catsSyntaxApplicativeId
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monad.*
import cats.syntax.traverse.*
import fs2.io.file.{Files, Path}
import scribe.Logging

import scala.util.Try

class AquaFileSources[F[_]: AquaIO: Monad: Files: Functor](
  sourcesPath: Path,
  importFrom: List[Path]
) extends AquaSources[F, AquaFileError, FileModuleId] with Logging {
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
        Validated
          .invalidNec[AquaFileError, Chain[(FileModuleId, String)]](ListAquaErrors(e))
          .pure[F]
    }

  // Resolve an import that was written in a 'from' file
  // Try to find it in a list of given imports or near 'from' file
  override def resolveImport(
    from: FileModuleId,
    imp: String
  ): F[ValidatedNec[AquaFileError, FileModuleId]] = {
    val validatedPath = Validated.fromEither(Try(Path(imp)).toEither.leftMap(FileSystemError.apply))
    validatedPath match {
      case Validated.Valid(importP) =>
        filesIO
          .resolve(importP, importFrom.prependedAll(from.file.parent))
          .bimap(NonEmptyChain.one, FileModuleId(_))
          .value
          .map(Validated.fromEither)
      case Validated.Invalid(err) => Validated.invalidNec[AquaFileError, FileModuleId](err).pure[F]
    }

  }

  override def load(file: FileModuleId): F[ValidatedNec[AquaFileError, String]] =
    filesIO.readFile(file.file).leftMap(NonEmptyChain.one).value.map(Validated.fromEither)

  // Get a directory of a file, or this file if it is a directory itself
  private def getDir(path: Path): F[Path] = {
    Files[F]
      .isDirectory(path)
      .map { res =>
        if (res) path else path.parent.getOrElse(path)
      }
  }

  /**
   * @param srcFile aqua source
   * @param targetPath a main path where all output files will be written
   * @param suffix `.aqua` will be replaced with this suffix
   * @return
   */
  def resolveTargetPath(
    srcFile: Path,
    targetPath: Path,
    suffix: String
  ): F[Validated[Throwable, Path]] =
    getDir(sourcesPath).map { srcDir =>
      Validated.catchNonFatal {
        val srcFilePath = srcDir.absolute.normalize
          .relativize(srcFile.absolute.normalize)

        val targetDir =
          targetPath.absolute.normalize
            .resolve(
              srcFilePath
            )

        targetDir.parent
          .getOrElse(targetDir)
          .resolve(srcFile.fileName.toString.stripSuffix(".aqua") + suffix)
      }
    }

  // Write content to a file and return a success message
  private def writeWithResult(target: Path, content: String, size: Int) = {
    filesIO
      .writeFile(
        target,
        content
      )
      .as(s"Result $target: compilation OK ($size functions)")
      .value
      .map(Validated.fromEither)
  }

  def write(
    targetPath: Path
  )(ac: AquaCompiled[FileModuleId]): F[Seq[Validated[AquaFileError, String]]] =
    if (ac.compiled.isEmpty)
      Seq(
        Validated.valid[AquaFileError, String](
          s"Source ${ac.sourceId.file}: compilation OK (nothing to emit)"
        )
      ).pure[F]
    else
      ac.compiled.map { compiled =>
        resolveTargetPath(
          ac.sourceId.file,
          targetPath,
          compiled.suffix
        ).flatMap { result =>
          result
            .leftMap(FileSystemError.apply)
            .map { target =>
              writeWithResult(target, compiled.content, ac.compiled.size)
            }
            .traverse(identity)
        }
      }.traverse(identity)
        .map(_.map(_.andThen(identity)))
}
