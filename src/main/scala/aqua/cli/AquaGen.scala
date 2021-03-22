package aqua.cli

import aqua.{Aqua, AquaError}
import cats.data.{NonEmptyList, Validated}
import cats.effect.Concurrent
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.Applicative
import fs2.io.file.Files
import fs2.text

import java.io.File
import java.nio.file.Path

case class ErrorInfo(name: String, script: String, errors: NonEmptyList[AquaError])

object AquaGen {

  def convertAquaFromFile[F[_]: Files: Concurrent](
    file: File,
    outputDir: Path
  ): F[Either[ErrorInfo, String]] = {
    val name = file.getName
    for {
      converted <- Files[F]
        .readAll(file.toPath, 4096)
        .through(text.utf8Decode)
        .map(text =>
          Aqua.generate(text) match {
            case Validated.Valid(v) ⇒
              Right(v)
            case Validated.Invalid(errs) ⇒
              Left(ErrorInfo(name, text, errs))
          }
        )
        .compile
        .toList
        .map(_.head)
      result <- {
        converted match {
          case Right(str) =>
            fs2.Stream
              .emit(str)
              .through(text.utf8Encode)
              .through(Files[F].writeAll(outputDir.resolve(name + ".ts")))
              .compile
              .drain
              .map(_ => Right(name))
          case Left(errs) =>
            Applicative[F].pure(Left(errs))
        }
      }
    } yield result
  }

  def convertAqua[F[_]: Files: Concurrent](
    files: List[File],
    outputDir: Path
  ): F[List[Either[ErrorInfo, String]]] =
    (for {
      file <- files
    } yield {
      convertAquaFromFile(file, outputDir)
    }).sequence
}
