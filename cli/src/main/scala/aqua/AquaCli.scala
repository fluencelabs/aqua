package aqua

import aqua.model.transform.BodyConfig
import cats.data.Validated
import cats.effect._
import cats.effect.std.Console
import cats.syntax.apply._
import cats.syntax.functor._
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import fs2.io.file.Files
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}
import wvlet.log.{LogSupport, Logger => WLogger}

object AquaCli extends IOApp with LogSupport {
  import AppOps._

  def main[F[_]: Concurrent: Files: Console: Logger]: Opts[F[ExitCode]] = {
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
      noRelay,
      noXorWrapper,
      wrapWithOption(helpOpt),
      wrapWithOption(versionOpt),
      logLevelOpt
    ).mapN { case (input, imports, output, toAir, noRelay, noXor, h, v, logLevel) =>
      WLogger.setDefaultLogLevel(LogLevel.toLogLevel(logLevel))

      // if there is `--help` or `--version` flag - show help and version
      // otherwise continue program execution
      h.map(_ => helpAndExit) orElse v.map(_ => versionAndExit) getOrElse
        AquaCompiler
          .compileFilesTo[F](
            input,
            imports,
            output,
            if (toAir) AquaCompiler.AirTarget else AquaCompiler.TypescriptTarget, {
              val bc = BodyConfig(wrapWithXor = !noXor)
              bc.copy(relayVarName = bc.relayVarName.filterNot(_ => noRelay))
            }
          )
          .map {
            case Validated.Invalid(errs) =>
              errs.map(println)
              ExitCode.Error
            case Validated.Valid(res) =>
              res.map(println)
              ExitCode.Success
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
      args
    )
  }
}
