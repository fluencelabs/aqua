package aqua

import aqua.backend.Backend
import aqua.backend.air.AirBackend
import aqua.backend.js.JavaScriptBackend
import aqua.backend.ts.TypeScriptBackend
import aqua.files.AquaFilesIO
import aqua.model.transform.BodyConfig
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.Id
import cats.data.Validated
import cats.effect._
import cats.effect.std.{Console => ConsoleEff}
import cats.syntax.apply._
import cats.syntax.functor._
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import fs2.io.file.Files
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}
import wvlet.log.LogFormatter.{appendStackTrace, highlightLog}
import wvlet.log.{LogFormatter, LogRecord, LogSupport, Logger => WLogger}

object CustomLogFormatter extends LogFormatter {

  override def formatLog(r: LogRecord): String = {
    val log =
      s"[${highlightLog(r.level, r.level.name)}] ${highlightLog(r.level, r.getMessage)}"
    appendStackTrace(log, r)
  }
}

object AquaCli extends IOApp with LogSupport {
  import AppOps._

  sealed trait CompileTarget
  case object TypescriptTarget extends CompileTarget
  case object JavaScriptTarget extends CompileTarget
  case object AirTarget extends CompileTarget

  def targetToBackend(target: CompileTarget): Backend = {
    target match {
      case TypescriptTarget =>
        TypeScriptBackend
      case JavaScriptTarget =>
        JavaScriptBackend
      case AirTarget =>
        AirBackend
    }
  }

  def main[F[_]: Concurrent: Files: ConsoleEff: Logger]: Opts[F[ExitCode]] = {
    versionOpt
      .as(
        versionAndExit
      ) orElse helpOpt.as(
      helpAndExit
    ) orElse (
      inputOpts,
      importOpts,
      outputOpts,
      compileToAir,
      compileToJs,
      noRelay,
      noXorWrapper,
      wrapWithOption(helpOpt),
      wrapWithOption(versionOpt),
      logLevelOpt,
      constantOpts[Id]
    ).mapN {
      case (input, imports, output, toAir, toJs, noRelay, noXor, h, v, logLevel, constants) =>
        WLogger.setDefaultLogLevel(LogLevel.toLogLevel(logLevel))
        WLogger.setDefaultFormatter(CustomLogFormatter)

        implicit val aio: AquaIO[F] = new AquaFilesIO[F]

        // if there is `--help` or `--version` flag - show help and version
        // otherwise continue program execution
        h.map(_ => helpAndExit) orElse v.map(_ => versionAndExit) getOrElse {
          val target =
            if (toAir) AirTarget
            else if (toJs) JavaScriptTarget
            else TypescriptTarget
          val bc = {
            val bc = BodyConfig(wrapWithXor = !noXor, constants = constants)
            bc.copy(relayVarName = bc.relayVarName.filterNot(_ => noRelay))
          }
          info(s"Aqua Compiler ${versionStr}")
          AquaPathCompiler
            .compileFilesTo[F](
              input,
              imports,
              output,
              targetToBackend(target),
              bc
            )
            .map {
              case Validated.Invalid(errs) =>
                errs.map(println)
                ExitCode.Error
              case Validated.Valid(results) =>
                results.map(println)
                ExitCode.Success
            }
        }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {

    implicit def logger[F[_]: Sync]: SelfAwareStructuredLogger[F] =
      Slf4jLogger.getLogger[F]

    CommandIOApp.run[IO](
      "aqua-c",
      "Aquamarine compiler",
      helpFlag = false,
      None
    )(
      main[IO],
      // Weird ugly hack: in case version flag or help flag is present, ignore other options,
      // be it correct or not
      args match {
        case _ if args.contains("-v") || args.contains("--version") => "-v" :: Nil
        case _ if args.contains("-h") || args.contains("--help") => "-h" :: Nil
        case _ => args
      }
    )
  }
}
