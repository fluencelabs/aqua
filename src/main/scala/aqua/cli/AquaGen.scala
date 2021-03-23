package aqua.cli

import aqua.{Aqua, AquaError}
import cats.data.{EitherT, NonEmptyChain, Validated}
import cats.effect.Concurrent
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Functor, Monad}
import fs2.io.file.Files
import fs2.text

import java.io.File
import java.nio.file.{Path, Paths}

sealed trait CliError

object CliError {

  def parseError(name: String, error: String): CliError = {
    ParseError(name, error)
  }

  def errorInfo(name: String, script: String, errors: NonEmptyChain[AquaError]): CliError = {
    ErrorInfo(name, script, errors)
  }

  def ioError(msg: String, t: Throwable): CliError = {
    IOError(msg, t)
  }
}

case class IOError(msg: String, t: Throwable) extends Exception(msg, t) with CliError
case class ParseError(name: String, error: String) extends CliError
case class ErrorInfo(name: String, script: String, errors: NonEmptyChain[AquaError]) extends CliError

object AquaGen {

  def checkAndChangeExtension[F[_]: Monad](fileName: String): EitherT[F, CliError, String] = {
    val arr = fileName.split("\\.").toList
    for {
      _ <- EitherT.cond[F](
        arr.nonEmpty && arr.last == "aqua",
        (),
        CliError.parseError(fileName, s"File '$fileName' should have '.aqua' extension")
      )
    } yield {
      arr.dropRight(1).mkString(".") + ".ts"
    }
  }

  def convertAquaFromFile[F[_]: Files: Concurrent](
    file: File,
    outputDir: Path
  ): EitherT[F, CliError, String] = {
    val name = file.getName
    for {
      newName <- checkAndChangeExtension(name)
      newPath = outputDir.resolve(newName)
      converted <- EitherT(
        Files[F]
          .readAll(file.toPath, 4096)
          .through(text.utf8Decode)
          .attempt
          .map {
            _.left
              .map(t => CliError.ioError("Error on reading file", t))
              .flatMap { text =>
                Aqua.generate(text) match {
                  case Validated.Valid(v) ⇒
                    println("success:" + v)
                    Right(v)
                  case Validated.Invalid(errs) ⇒
                    Left(CliError.errorInfo(name, text, errs))
                }
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

  def convertAqua[F[_]: Files: Concurrent](
    files: List[File],
    outputDir: Path
  ): F[List[Either[CliError, String]]] = {
    fs2.Stream
      .emits(files)
      .evalMap(f => convertAquaFromFile(f, outputDir).value)
      .compile
      .toList
  }
}
