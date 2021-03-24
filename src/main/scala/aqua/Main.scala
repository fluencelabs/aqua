package aqua

import aqua.cli.AquaGen.{convertAqua, convertAquaFilesToDir}
import aqua.cli.{AquaScriptErrors, CliArgsError, CliError, IOError}
import cats.effect.{ExitCode, IO, IOApp, Resource}
import scopt.OParser

import java.io.File
import java.nio.file.Path

final case class ParseArgsException(private val message: String, private val cause: Throwable = None.orNull)
    extends Exception(message, cause)

case class Config(input: Option[String] = None, output: Option[String] = None, debug: Boolean = false)

object Main extends IOApp {

  private val builder = OParser.builder[Config]

  private val parser1 = {
    import builder._
    OParser.sequence(
      programName("ahc"),
      head("ahc", "0.1"),
      opt[Boolean]('d', "debug")
        .action((x, c) => c.copy(debug = x))
        .text("debug mode (not implemented)"),
      opt[String]('i', "input")
        .action((x, c) => c.copy(input = Some(x)))
        .text("path to directory with aquamarine files"),
      opt[String]('o', "output")
        .action((x, c) => c.copy(output = Some(x)))
        .text("path to output directory"),
      help('h', "help").text("prints this usage text"),
      checkConfig(c => {
        println(c.input)
        println(c.output)
        if (c.input.isEmpty != c.output.isEmpty) {
          failure("'input' and 'output' must be both specified or not specified")
        } else success
      })
    )
  }

  private def parseIO(input: String, output: String): Either[ParseArgsException, (List[File], Path)] = {
    for {
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

  private def showResults(results: List[Either[CliError, String]]): IO[Unit] = {
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

  private def readAllInput(): IO[String] = {
    import java.io.BufferedReader
    import java.io.InputStreamReader
    Resource.make(IO(new BufferedReader(new InputStreamReader(System.in))))(b => IO(b.close())).use { reader =>
      IO {
        if (reader.ready()) {
          val lineSep = sys.props("line.separator")
          var line: String = reader.readLine
          val buf = new StringBuilder()
          while (line != null) {
            buf.append(line + lineSep)
            line = reader.readLine
          }
          buf.toString
        } else ""
      }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    OParser.parse(parser1, args, Config()) match {
      case Some(config) =>
        val io = for {
          results <- {
            (config.input, config.output) match {
              case (Some(i), Some(o)) =>
                IO.fromEither(parseIO(i, o)).flatMap { case (files, outputDir) =>
                  convertAquaFilesToDir[IO](files, outputDir)
                }
              case _ =>
                readAllInput().map(i => {
                  if (i.isEmpty) {
                    println("input is empty")
                    List()
                  } else List(convertAqua(i))
                })

            }
          }
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
      case _ =>
        // errors should have been reported before
        IO(ExitCode.Error)
    }
  }
}
