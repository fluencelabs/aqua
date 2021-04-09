package aqua

import aqua.cli.AquaGen.{convertAqua, convertAquaFilesToDir}
import aqua.cli.{AquaScriptErrors, ArgsConfig, CliArgsError, CliError, IOError}
import cats.effect.{ExitCode, IO, IOApp, Resource}

final case class ParseArgsException(
  private val message: String,
  private val cause: Throwable = None.orNull
) extends Exception(message, cause)

object Main extends IOApp {

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

  private def readAllInput(): IO[Option[String]] = {
    import java.io.BufferedReader
    import java.io.InputStreamReader
    Resource
      .make(IO(new BufferedReader(new InputStreamReader(System.in))))(b => IO(b.close()))
      .use { reader =>
        IO {
          if (reader.ready()) {
            val lineSep = sys.props("line.separator")
            var line: String = reader.readLine
            val buf = new StringBuilder()
            while (line != null) {
              buf.append(line + lineSep)
              line = reader.readLine
            }
            Some(buf.toString)
          } else Option("")
        }
      }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    ArgsConfig.parseArgs(args) match {
      case Some(config) =>
        val io = for {
          results <-
            (config.input, config.output) match {
              case (Some(i), Some(o)) =>
                convertAquaFilesToDir[IO](
                  // TODO this approach does not support nested directories
                  i.toFile.listFiles().toList.filter(_.getName.endsWith(".aqua")),
                  o,
                  config.air
                )
              case _ =>
                readAllInput().map {
                  case Some(i) =>
                    List(convertAqua("stdin", i, config.air))
                  case None =>
                    println("input is empty")
                    List()
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
