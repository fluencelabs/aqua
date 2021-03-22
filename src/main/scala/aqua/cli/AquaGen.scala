package aqua.cli

import aqua.Aqua
import cats.data.Validated
import cats.effect.IO
import cats.syntax.traverse._
import fs2.io.file.Files
import fs2.{text, Pipe}

import java.io.File
import java.nio.file.Path

object AquaGen {

  def convertAquaFromFile(file: File, outputDir: Path): IO[Unit] = {
    val name = file.getName
    for {
      converted <- Files[IO]
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
      _ <- converted match {
        case Some(str) =>
          fs2.Stream
            .eval(IO(str))
            .through(text.utf8Encode)
            .through(Files[IO].writeAll(outputDir.resolve(name + ".ts")))
            .compile
            .drain
        case None => IO.unit
      }
    } yield ()
  }

  def convertAqua(files: List[File], outputDir: Path): IO[List[Unit]] =
    (for {
      file <- files
    } yield {
      convertAquaFromFile(file, outputDir)
    }).sequence
}
