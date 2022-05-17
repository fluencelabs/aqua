package aqua.files

import aqua.AquaIO
import aqua.io.*
import cats.data.*
import cats.data.Validated.{Invalid, Valid}
import cats.effect.kernel.Concurrent
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.foldable.*
import fs2.io.file.{Files, Path}
import fs2.text

import scala.util.Try

class AquaFilesIO[F[_]: Files: Concurrent] extends AquaIO[F] {

  override def readFile(file: Path): EitherT[F, AquaFileError, String] =
    EitherT(
      Files[F]
        .readAll(file)
        .fold(Vector.empty[Byte])((acc, b) => acc :+ b)
        // TODO fix for comment on last line in air
        // TODO should be fixed by parser
        .map(_.appendedAll("\n\r".getBytes))
        .flatMap(fs2.Stream.emits)
        .through(text.utf8.decode)
        .attempt
        .compile
        .last
        .map(
          _.fold((EmptyFileError(file): AquaFileError).asLeft[String])(
            _.left.map(FileSystemError.apply)
          )
        )
    )

  /**
   * Find the first file that exists in the given list of paths
   * If there is no such file - error
   */
  private def findFirstF(
    in: List[Path],
    notFound: EitherT[F, AquaFileError, Path]
  ): EitherT[F, AquaFileError, Path] =
    in.headOption.fold(notFound)(p =>
      EitherT(
        Concurrent[F].attempt(Files[F].isRegularFile(p))
      )
        .leftMap[AquaFileError](FileSystemError.apply)
        .recover({ case _ => false })
        .flatMap {
          case true =>
            EitherT(
              Concurrent[F].attempt(p.absolute.normalize.pure[F])
            ).leftMap[AquaFileError](FileSystemError.apply)
          case false =>
            findFirstF(in.tail, notFound)
        }
    )

  /**
   * Checks if a file exists in the list of possible paths
   */
  def resolve(
    src: Path,
    imports: List[Path]
  ): EitherT[F, AquaFileError, Path] =
    findFirstF(
      imports
        .map(_.resolve(src)),
      EitherT.leftT(FileNotFound(src, imports))
    )

  // Get all files for every path if the path in the list is a directory or this path otherwise
  private def gatherFiles(
    files: List[Path],
    listFunction: (f: Path) => F[ValidatedNec[AquaFileError, Chain[Path]]]
  ): List[F[ValidatedNec[AquaFileError, Chain[Path]]]] = {
    files.map(f => gatherFile(f, listFunction))
  }

  // Get all files if the path is a directory or this path otherwise
  private def gatherFile(
    f: Path,
    listFunction: (f: Path) => F[ValidatedNec[AquaFileError, Chain[Path]]]
  ): F[ValidatedNec[AquaFileError, Chain[Path]]] = {
    Files[F].isDirectory(f).flatMap { isDir =>
      if (isDir)
        listFunction(f)
      else
        Files[F].isRegularFile(f).map { isFile =>
          if (isFile)
            Validated.validNec(Chain.one(f.absolute.normalize))
          else
            Validated.invalidNec(FileNotFound(f, Nil))
        }
    }
  }

  // Get all files if the path is a directory or this path otherwise
  override def listAqua(folder: Path): F[ValidatedNec[AquaFileError, Chain[Path]]] = {
    Files[F]
      .exists(folder)
      .flatMap { exists =>
        if (!exists) {
          Left(FileNotFound(folder, Nil): AquaFileError).pure[F]
        } else {
          Files[F].isDirectory(folder).flatMap { isDir =>
            if (isDir) {
              Files[F]
                .list(folder)
                .evalFilter(p =>
                  if (p.extName == ".aqua") true.pure[F]
                  else Files[F].isDirectory(p)
                )
                .compile
                .toList
                .map(Right(_))
            } else {
              Right(folder :: Nil).pure[F]
            }
          }
        }
      }
      .map(Validated.fromEither)
      .map(_.leftMap(NonEmptyChain.one))
      .flatMap {
        case Valid(files) =>
          gatherFiles(files, listAqua).foldLeft(
            Validated.validNec[AquaFileError, Chain[Path]](Chain.nil).pure[F]
          ) { case (acc, v) =>
            (acc, v).mapN(_ combine _)
          }
        case Invalid(errs) =>
          Validated.invalid[NonEmptyChain[AquaFileError], Chain[Path]](errs).pure[F]
      }
  }

  private def deleteIfExists(file: Path): EitherT[F, AquaFileError, Boolean] =
    Files[F].deleteIfExists(file).attemptT.leftMap(FileSystemError.apply)

  private def createDirectories(path: Path): EitherT[F, AquaFileError, Unit] =
    Files[F].createDirectories(path).attemptT.leftMap(FileSystemError.apply(_): AquaFileError)

  // Writes to a file, creates directories if they do not exist
  override def writeFile(file: Path, content: String): EitherT[F, AquaFileError, Unit] =
    deleteIfExists(file) >> file.parent
      .map(createDirectories)
      .getOrElse(EitherT.liftF(().pure[F])) >>
      EitherT(
        fs2.Stream
          .emit(content)
          .through(text.utf8.encode)
          .through(Files[F].writeAll(file))
          .attempt
          .compile
          .last
          .map(_.getOrElse(Right(())))
      )
        .leftMap(FileWriteError(file, _))

}

object AquaFilesIO {
  implicit def summon[F[_]: Files: Concurrent]: AquaIO[F] = new AquaFilesIO[F]
}
