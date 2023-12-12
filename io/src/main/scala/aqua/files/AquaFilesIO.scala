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
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
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
   * Return first path that is a regular file
   */
  override def resolve(
    paths: List[Path]
  ): EitherT[F, AquaFileError, Path] =
    paths
      .collectFirstSomeM(p =>
        Concurrent[F]
          .attemptT(Files[F].isRegularFile(p))
          .recover(_ => false)
          .leftMap(FileSystemError.apply)
          .map(Option.when(_)(p))
      )
      .flatMap {
        case None =>
          EitherT.leftT(
            FilesUnresolved(paths)
          )
        case Some(p) =>
          Try(
            p.absolute.normalize
          ).toEither.leftMap(FileSystemError.apply).toEitherT
      }

  // Get all files if the path is a directory or this path otherwise
  override def listAqua(folder: Path): EitherT[F, AquaFileError, Chain[Path]] =
    for {
      exists <- EitherT.liftF(Files[F].exists(folder))
      _ <- EitherT.cond(exists, (), FileNotFound(folder): AquaFileError)
      paths <- EitherT.liftF(
        Files[F]
          .walk(folder)
          .evalFilter(p =>
            Files[F]
              .isRegularFile(p)
              .map(_ && p.extName == ".aqua")
          )
          .compile
          .toList
      )
    } yield Chain.fromSeq(paths)

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
