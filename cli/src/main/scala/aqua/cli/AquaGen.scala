package aqua.cli

import aqua.Aqua
import cats.data.{EitherT, Validated}
import cats.effect.Concurrent
import cats.syntax.functor._
import cats.Applicative
import fs2.io.file.Files
import fs2.text

import java.io.File
import java.nio.file.Path

object AquaGen {

  def checkAndChangeExtension[F[_]: Applicative](
    fileName: String,
    air: Boolean
  ): EitherT[F, CliError, String] = {
    val arr = fileName.split("\\.").toList
    for {
      _ <- EitherT.cond[F](
        arr.nonEmpty && arr.last == "aqua",
        (),
        CliError.parseError(fileName, s"File '$fileName' should have '.aqua' extension")
      )
    } yield {
      arr.dropRight(1).mkString(".") + (if (air) ".air" else ".ts")
    }
  }

  def convertAqua[F[_]](name: String, text: String, air: Boolean): Either[CliError, String] = {
    Aqua.generate(text, air) match {
      case Validated.Valid(v) ⇒
        Right(v)
      case Validated.Invalid(errs) ⇒
        Left(CliError.errorInfo(name, text, errs))
    }
  }

  def convertAquaFromFile[F[_]: Files: Concurrent](
    file: File,
    outputDir: Path,
    air: Boolean
  ): EitherT[F, CliError, String] = {
    val name = file.getName
    for {
      newName <- checkAndChangeExtension(name, air)
      newPath = outputDir.resolve(newName)
      converted <- EitherT(
        Files[F]
          .readAll(file.toPath, 4096)
          .through(text.utf8Decode)
          .fold("")((acc, str) => acc + str)
          .attempt
          .map {
            _.left
              .map(t => CliError.ioError("Error on reading file", t))
              .flatMap { text =>
                convertAqua(name, text, air)
              }
          }
          .compile
          .toList
          .map(_.head)
      )
      // delete old file
      _ <- EitherT.right(Files[F].deleteIfExists(newPath))
      result <-
        EitherT[F, CliError, String](
          fs2.Stream
            .emit(converted)
            .through(text.utf8Encode)
            .through(Files[F].writeAll(newPath))
            .attempt
            .map { e =>
              e.left
                .map(t => CliError.ioError("Error on writing file", t))
            }
            .compile
            .drain
            .map(_ => Right(newName))
        )
    } yield result
  }

  def convertAquaFilesToDir[F[_]: Files: Concurrent](
    files: List[File],
    outputDir: Path,
    air: Boolean
  ): F[List[Either[CliError, String]]] =
    fs2.Stream
      .emits(files)
      .evalMap(f => convertAquaFromFile(f, outputDir, air).value)
      .compile
      .toList
}
