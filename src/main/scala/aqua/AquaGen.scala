package aqua

import cats.data.Validated
import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Files
import fs2.{text, Stream}
import cats.implicits._

import java.io.{File, PrintWriter}
import scala.io.Source
import java.nio.file.{Path, Paths}

final case class ParseArgsException(private val message: String, private val cause: Throwable = None.orNull)
    extends Exception(message, cause)

object AquaGen extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    val io = for {
      args <- IO.fromEither(parseArgs(args))
      (input, output) = args
      _ <- convertAqua(input, output)
    } yield {}
    io.map(_ => ExitCode.Success)
      .handleErrorWith(err => {
        println(err)
        IO(ExitCode.Error)
      })
  }

  def parseArgs(args: List[String]): Either[ParseArgsException, (List[File], Path)] = {
    val error = ParseArgsException("There should be two arguments: path/to/input/dir and path/to/output/dir")
    for {
      input <- args.headOption.toRight(error)
      output <- args.lift(1).toRight(error)
      inputDir <- {
        val inputDir = new File(input)
        if (!inputDir.isDirectory && !inputDir.exists())
          Left(ParseArgsException("Input path should be a dir and exists"))
        else Right(inputDir)
      }
      outputDir <- {
        val outputDir = new File(output)
        if (!outputDir.isDirectory && !outputDir.exists()) Left(ParseArgsException("Output path should be a dir"))
        else Right(outputDir)
      }

    } yield (inputDir.listFiles().toList, outputDir.toPath)
  }

  def convertAqua(files: List[File], outputDir: Path): IO[List[Unit]] = {
    (for {
      file <- files
    } yield {
      Files[IO]
        .readAll(file.toPath, 4096)
        .through(text.utf8Decode)
        .map(text =>
          Aqua.generate(text) match {
            case Validated.Valid(v) ⇒
              v.mkString("\n")
            case Validated.Invalid(errs) ⇒
              errs.map(_.showForConsole(text)).toList.mkString("\n")
          }
        )
        .through(text.utf8Encode)
        .through(Files[IO].writeAll(outputDir.resolve(file.getName + ".result")))
        .compile
        .drain
    }).sequence
  }
}
