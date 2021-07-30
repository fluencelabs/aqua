package aqua.files

import aqua.AquaIO
import aqua.io._
import cats.data.Validated.{Invalid, Valid}
import cats.data._
import cats.effect.kernel.Concurrent
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import fs2.io.file.Files
import fs2.text

import java.nio.file.Path
import scala.util.Try

class AquaFilesIO[F[_]: Files: Concurrent] extends AquaIO[F] {

  override def readFile(file: Path): EitherT[F, AquaFileError, String] =
    EitherT(
      Files[F]
        .readAll(file, 4096)
        .fold(Vector.empty[Byte])((acc, b) => acc :+ b)
        // TODO fix for comment on last line in air
        // TODO should be fixed by parser
        .map(_.appendedAll("\n\r".getBytes))
        .flatMap(fs2.Stream.emits)
        .through(text.utf8Decode)
        .attempt
        .compile
        .last
        .map(
          _.fold((EmptyFileError(file): AquaFileError).asLeft[String])(_.left.map(FileSystemError))
        )
    )

  // Find first existed file in list of paths
  // If there is no existed files - throw an error
  private def findFirstF(
    in: List[Path],
    notFound: EitherT[F, AquaFileError, Path]
  ): EitherT[F, AquaFileError, Path] =
    in.headOption.fold(notFound)(p =>
      EitherT(
        Concurrent[F].attempt(p.toFile.isFile.pure[F])
      )
        .leftMap[AquaFileError](FileSystemError)
        .recover({ case _ => false })
        .flatMap {
          case true =>
            EitherT(
              Concurrent[F].attempt(p.toAbsolutePath.normalize().pure[F])
            ).leftMap[AquaFileError](FileSystemError)
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

  override def listAqua(folder: Path): F[ValidatedNec[AquaFileError, Chain[Path]]] =
    Validated
      .fromEither(
        Try {
          val f = folder.toFile
          if (!f.exists()) {
            Left(FileNotFound(folder, Nil))
          } else if (f.isDirectory) {
            Right(f.listFiles().toList)
          } else {
            Right(f :: Nil)
          }
        }.toEither.leftMap[AquaFileError](FileSystemError).flatMap(identity)
      )
      .leftMap(NonEmptyChain.one)
      .pure[F]
      .flatMap {
        case Valid(files) =>
          files.collect {
            case f if f.isFile && f.getName.endsWith(".aqua") =>
              Validated
                .fromTry(
                  Try(Chain.one(f.toPath.toAbsolutePath.normalize()))
                )
                .leftMap(FileSystemError)
                .leftMap(NonEmptyChain.one)
                .pure[F]
            case f if f.isDirectory =>
              listAqua(f.toPath)
          }.foldLeft(Validated.validNec[AquaFileError, Chain[Path]](Chain.nil).pure[F]) {
            case (acc, v) =>
              (acc, v).mapN(_ combine _)
          }
        case Invalid(errs) =>
          Validated.invalid[NonEmptyChain[AquaFileError], Chain[Path]](errs).pure[F]
      }

  // Writes to a file, creates directories if they do not exist
  override def writeFile(file: Path, content: String): EitherT[F, AquaFileError, Unit] =
    EitherT
      .right[AquaFileError](Files[F].deleteIfExists(file))
      .flatMap(_ => EitherT.right[AquaFileError](Files[F].createDirectories(file.getParent)))
      .flatMap(_ =>
        EitherT[F, AquaFileError, Unit](
          fs2.Stream
            .emit(content)
            .through(text.utf8Encode)
            .through(Files[F].writeAll(file))
            .attempt
            .map { e =>
              e.left
                .map(t => FileWriteError(file, t))
            }
            .compile
            .last
            .map(res => res.getOrElse(Right()))
        )
      )
}

object AquaFilesIO {
  implicit def summon[F[_]: Files: Concurrent]: AquaIO[F] = new AquaFilesIO[F]
}
