package aqua.cli

import aqua.Aqua
import cats.data.Validated
import cats.effect.Concurrent
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Functor, Monad}
import fs2.io.file.Files
import fs2.text

import java.io.File
import java.nio.file.Path

object AquaGen {

  def convertAquaFromFile[F[_]: Files: Concurrent: Functor: Monad](file: File, outputDir: Path): F[Unit] = {
    val name = file.getName
    for {
      converted <- Files[F]
        .readAll(file.toPath, 4096)
        .through(text.utf8Decode)
        .map(text =>
          Aqua.generate(text) match {
            case Validated.Valid(v) ⇒
              println(Console.GREEN + s"File '$name' processed successfully" + Console.RESET)
              Some(v)
            case Validated.Invalid(errs) ⇒
              println(Console.RED + s"File '$name' processed with errors:" + Console.RESET)
              errs.map(_.showForConsole(text)).map(println)
              None
          }
        )
        .collect { case Some(s) => s }
        .compile
        .toList
        .map(_.headOption)
      _ <- {
        converted match {
          case Some(str) =>
            fs2.Stream
              .emit(str)
              .through(text.utf8Encode)
              .through(Files[F].writeAll(outputDir.resolve(name + ".ts")))
              .compile
              .drain
          case None => Applicative[F].unit
        }
      }
    } yield ()
  }

  def convertAqua[F[_]: Files: Concurrent: Functor: Monad](files: List[File], outputDir: Path): F[List[Unit]] =
    (for {
      file <- files
    } yield {
      convertAquaFromFile(file, outputDir)
    }).sequence
}
