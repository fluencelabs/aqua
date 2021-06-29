package aqua

import aqua.compiler.AquaIO
import aqua.compiler.io.{
  AquaFileError,
  EmptyFileError,
  FileNotFound,
  FileSystemError,
  FileWriteError
}
import aqua.parser.lift.FileSpan
import cats.data.Validated.{Invalid, Valid}
import cats.data.{Chain, EitherT, NonEmptyChain, Validated, ValidatedNec}
import cats.syntax.functor._
import cats.syntax.either._
import cats.effect.kernel.Concurrent
import fs2.io.file.Files
import fs2.text
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.apply._

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
    focus: FileSpan.Focus,
    src: Path,
    imports: List[Path]
  ): EitherT[F, AquaFileError, Path] =
    findFirstF(
      imports
        .map(_.resolve(src)),
      EitherT.leftT(FileNotFound(focus, src, imports))
    )

  override def listAqua(folder: Path): F[ValidatedNec[AquaFileError, Chain[Path]]] =
    Validated
      .fromTry(
        Try {
          val f = folder.toFile
          if (f.isDirectory) {
            f.listFiles().toList
          } else {
            f :: Nil
          }
        }
      )
      .leftMap[AquaFileError](FileSystemError)
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

  override def writeFile(file: Path, content: String): EitherT[F, AquaFileError, Unit] =
    EitherT
      .right[AquaFileError](Files[F].deleteIfExists(file))
      .flatMap(_ =>
        EitherT[F, AquaFileError, Unit](
          fs2.Stream
            .emit(
              content
            )
            .through(text.utf8Encode)
            .through(Files[F].writeAll(file))
            .attempt
            .map { e =>
              e.left
                .map(t => FileWriteError(file, t))
            }
            .compile
            .drain
            .map(_ => Right(()))
        )
      )
}

object AquaFilesIO {
  implicit def summon[F[_]: Files: Concurrent]: AquaIO[F] = new AquaFilesIO[F]
}
