package aqua

import aqua.cli.AquaGen.convertAqua
import aqua.cli.{AquaScriptErrors, CliArgsError, CliError, IOError}
import cats.effect.{ExitCode, IO, IOApp}
import fs2.text

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

  def showResults(results: List[Either[CliError, String]]): IO[Unit] = {
    IO {
      results.map {
        case Left(err) =>
          err match {
            case AquaScriptErrors(name, script, errors) =>
              println(Console.RED + s"File '$name' processed with errors:" + Console.RESET)
              errors.map(_.showForConsole(script)).map(println)
            case CliArgsError(name, error) =>
              println(Console.RED + s"File '$name' processed with error: $error" + Console.RESET)
            case IOError(msg, t) =>
              println(Console.RED + s"$msg: $t" + Console.RESET)
          }

        case Right(name) =>
          println(Console.GREEN + s"File '$name' processed successfully" + Console.RESET)
      }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val io = for {
      args <- IO.fromEither(parseArgs(args))
      (input, output) = args
      results <- convertAqua[IO](input, output)
      _ <- showResults(results)
    } yield {
      if (results.exists(_.isLeft))
        ExitCode.Error
      else
        ExitCode.Success
    }
    io.handleErrorWith { err =>
      // this is an unhandled errors
      println(err)
      IO(ExitCode.Error)
    }
  }
}
