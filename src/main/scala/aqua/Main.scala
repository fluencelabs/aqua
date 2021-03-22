package aqua

import aqua.cli.AquaGen.{convertAqua}
import cats.effect.{ExitCode, IO, IOApp}

import java.io.File
import java.nio.file.Path

final case class ParseArgsException(private val message: String, private val cause: Throwable = None.orNull)
    extends Exception(message, cause)

object Main extends IOApp {

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

  override def run(args: List[String]): IO[ExitCode] = {
    val io = for {
      args <- IO.fromEither(parseArgs(args))
      (input, output) = args
      _ <- convertAqua(input, output)
    } yield ()
    io.map(_ => ExitCode.Success).handleErrorWith { err =>
      println(err)
      IO(ExitCode.Error)
    }
  }
}
