package aqua

import aqua.backend.Backend
import aqua.backend.air.AirBackend
import aqua.backend.js.JavaScriptBackend
import aqua.backend.ts.TypeScriptBackend
import aqua.files.AquaFilesIO
import aqua.model.result.transform.GenerationConfig
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.Id
import cats.data.Validated
import cats.effect.*
import cats.effect.std.Console as ConsoleEff
import cats.syntax.apply.*
import cats.syntax.functor.*
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import fs2.io.file.Files
import scribe.Logging

object AquaCli extends IOApp with Logging {
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

  def main[F[_]: Concurrent: Files: ConsoleEff](runtime: unsafe.IORuntime): Opts[F[ExitCode]] = {
    implicit val r = runtime
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
        scribe.Logger.root
          .clearHandlers()
          .clearModifiers()
          .withHandler(formatter = LogFormatter.formatter, minimumLevel = Some(logLevel))
          .replace()

        implicit val aio: AquaIO[F] = new AquaFilesIO[F]

        // if there is `--help` or `--version` flag - show help and version
        // otherwise continue program execution
        h.map(_ => helpAndExit) orElse v.map(_ => versionAndExit) getOrElse {
          val target =
            if (toAir) AirTarget
            else if (toJs) JavaScriptTarget
            else TypescriptTarget
          val bc = {
            val bc = GenerationConfig(wrapWithXor = !noXor, constants = constants)
            bc.copy(relayVarName = bc.relayVarName.filterNot(_ => noRelay))
          }
          logger.info(s"Aqua Compiler ${versionStr}")
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
                errs.map(System.out.println)
                ExitCode.Error
              case Validated.Valid(results) =>
                results.map(logger.info(_))
                ExitCode.Success
            }
        }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    CommandIOApp.run[IO](
      "aqua-c",
      "Aquamarine compiler",
      helpFlag = false,
      None
    )(
      main[IO](runtime),
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
