package aqua

import aqua.backend.Backend
import aqua.backend.air.AirBackend
import aqua.backend.js.JavaScriptBackend
import aqua.backend.ts.TypeScriptBackend
import aqua.files.AquaFilesIO
import aqua.model.transform.TransformConfig
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.{Functor, Id, Monad}
import cats.data.{Chain, NonEmptyList, Validated, ValidatedNec, ValidatedNel}
import cats.effect.*
import cats.effect.std.Console as ConsoleEff
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import com.monovore.decline.Opts
import com.monovore.decline
import com.monovore.decline.effect.CommandIOApp
import fs2.io.file.Files
import scribe.Logging
import cats.~>

import scala.concurrent.Future

object AquaCli extends IOApp with Logging {
  import AppOpts.*

  sealed trait CompileTarget
  case object TypescriptTarget extends CompileTarget
  case object JavaScriptTarget extends CompileTarget
  case object AirTarget extends CompileTarget

  def targetToBackend(target: CompileTarget): Backend = {
    target match {
      case TypescriptTarget =>
        TypeScriptBackend
      case JavaScriptTarget =>
        JavaScriptBackend(false)
      case AirTarget =>
        AirBackend
    }
  }

  def main[F[_]: Concurrent: Files: ConsoleEff](runtime: unsafe.IORuntime)(implicit F: Future ~> F): Opts[F[ExitCode]] = {
    implicit val r = runtime
    implicit val aio: AquaIO[F] = new AquaFilesIO[F]

    runCom orElse versionOpt
      .as(
        versionAndExit
      ) orElse helpOpt.as(
      helpAndExit
    ) orElse (
      inputOpts[F],
      importOpts[F],
      outputOpts[F],
      compileToAir,
      compileToJs,
      noRelay,
      noXorWrapper,
      wrapWithOption(helpOpt),
      wrapWithOption(versionOpt),
      logLevelOpt,
      constantOpts[Id],
      dryOpt
    ).mapN {
      case (inputF, importsF, outputF, toAir, toJs, noRelay, noXor, h, v, logLevel, constants, isDryRun) =>
        scribe.Logger.root
          .clearHandlers()
          .clearModifiers()
          .withHandler(formatter = LogFormatter.formatter, minimumLevel = Some(logLevel))
          .replace()

        // if there is `--help` or `--version` flag - show help and version
        // otherwise continue program execution
        h.map(_ => helpAndExit) orElse v.map(_ => versionAndExit) getOrElse {
          val target =
            if (toAir) AirTarget
            else if (toJs) JavaScriptTarget
            else TypescriptTarget
          val bc = {
            val bc = TransformConfig(wrapWithXor = !noXor, constants = constants)
            bc.copy(relayVarName = bc.relayVarName.filterNot(_ => noRelay))
          }
          logger.info(s"Aqua Compiler ${versionStr}")

          (inputF, outputF, importsF).mapN {(i, o, imp) =>
            i.andThen { input =>
              o.andThen { output =>
                imp.map { imports =>
                  if (output.isEmpty && !isDryRun)
                    Validated.invalidNec(
                      "Output path should be specified ('--output' or '-o'). " +
                        "Add '--dry' to check compilation without output")
                      .pure[F]
                  else {
                    val resultOutput = if (isDryRun) {
                      None
                    } else {
                      output
                    }
                    AquaPathCompiler
                      .compileFilesTo[F](
                        input,
                        imports,
                        resultOutput,
                        targetToBackend(target),
                        bc
                      )
                  }
                }
              }
            }
          }.flatMap {
            case Validated.Invalid(errs) =>
              errs.map(logger.error(_))
              ExitCode.Error.pure[F]
            case Validated.Valid(result) =>
              result.map {
                case Validated.Invalid(errs) =>
                  errs.map(logger.error(_))
                  ExitCode.Error
                case Validated.Valid(results) =>
                  results.map(logger.info(_))
                  ExitCode.Success
              }
          }
        }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    CommandIOApp.run[IO](
      "aqua",
      "Aqua Compiler",
      helpFlag = false,
      None
    )(
      {
        implicit val f = new (Future ~> IO) {
          override def apply[A](f: Future[A]): IO[A] =
            IO.fromFuture(IO(f))
        }
         main[IO](runtime)
      },
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
