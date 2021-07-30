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

  // Resolve an import that was written in a 'from' file
  // Try to find it in a list of given imports or near 'from' file
  override def resolve(
    from: FileModuleId,
    imp: String
  ): F[ValidatedNec[AquaFileError, FileModuleId]] = {
    Validated.fromEither(Try(Paths.get(imp)).toEither.leftMap(FileSystemError)) match {
      case Validated.Valid(importP) =>
        filesIO
          .resolve(importP, from.file.getParent +: importFrom)
          .bimap(NonEmptyChain.one, FileModuleId(_))
          .value
          .map(Validated.fromEither)
      case Validated.Invalid(err) => Validated.invalidNec[AquaFileError, FileModuleId](err).pure[F]
    }

  }

  override def load(file: FileModuleId): F[ValidatedNec[AquaFileError, String]] =
    filesIO.readFile(file.file).leftMap(NonEmptyChain.one).value.map(Validated.fromEither)

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
  ): Validated[Throwable, Path] =
    Validated.catchNonFatal {
      val srcDir = if (sourcesPath.toFile.isDirectory) sourcesPath else sourcesPath.getParent
      val srcFilePath = srcDir.toAbsolutePath
        .normalize()
        .relativize(srcFile.toAbsolutePath.normalize())

      val targetDir =
        targetPath.toAbsolutePath
          .normalize()
          .resolve(
            srcFilePath
          )

      targetDir.getParent.resolve(srcFile.getFileName.toString.stripSuffix(".aqua") + suffix)
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
        ).leftMap(FileSystemError)
          .map { target =>
            filesIO
              .writeFile(
                target,
                compiled.content
              )
              .as(s"Result $target: compilation OK (${ac.compiled.size} functions)")
              .value
              .map(Validated.fromEither)
          }
          .traverse(identity)
      }.traverse(identity)
        .map(_.map(_.andThen(identity)))
}
