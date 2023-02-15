package aqua

import aqua.backend.Backend
import aqua.backend.air.AirBackend
import aqua.backend.js.JavaScriptBackend
import aqua.backend.ts.TypeScriptBackend
import aqua.logging.LogFormatter
import aqua.files.AquaFilesIO
import aqua.io.OutputPrinter
import aqua.model.transform.TransformConfig
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.data.*
import cats.effect.*
import cats.effect.std.Console as ConsoleEff
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.{Functor, Id, Monad, ~>}
import com.monovore.decline
import com.monovore.decline.effect.CommandIOApp
import com.monovore.decline.effect.CommandIOApp.printHelp
import com.monovore.decline.{Command, Help, Opts, PlatformApp}
import fs2.io.file.Files
import scribe.Logging

import scala.concurrent.Future

object AquaCli extends IOApp with Logging {
  import AppOpts.*

  sealed trait CompileTarget
  case object TypescriptTarget extends CompileTarget
  case object JavaScriptTarget extends CompileTarget
  case object AirTarget extends CompileTarget

  def targetToBackend(target: CompileTarget, isOldFluenceJs: Boolean): Backend = {
    target match {
      case TypescriptTarget =>
        TypeScriptBackend(isOldFluenceJs)
      case JavaScriptTarget =>
        JavaScriptBackend(isOldFluenceJs)
      case AirTarget =>
        AirBackend
    }
  }

  def printErrorsOr[F[_]: Async: ConsoleEff, T](
    result: ValidatedNec[String, T],
    or: T => F[ExitCode]
  ): F[ExitCode] = {
    result match {
      case Validated.Invalid(errs) =>
        printCommandErrors[F](errs).as {
          ExitCode.Error
        }
      case Validated.Valid(results) =>
        or(results)
    }
  }

  def printCommandErrors[F[_]: ConsoleEff: Async](errs: NonEmptyChain[String]): F[Unit] = {
    errs.map(OutputPrinter.errorF _).sequence.flatMap { _ =>
      OutputPrinter.errorF("\nTry 'aqua --help' for usage instructions")
    }

  }

  def main[F[_]: Files: ConsoleEff: Async](runtime: unsafe.IORuntime): Opts[F[ExitCode]] = {
    implicit val r = runtime
    implicit val aio: AquaIO[F] = new AquaFilesIO[F]
    implicit val ec = r.compute

    PlatformOpts.opts.map { res =>
      res.flatMap {
        case Validated.Invalid(errs) =>
          printCommandErrors[F](errs).as {
            ExitCode.Error
          }
        case Validated.Valid(_) =>
          ExitCode.Success.pure[F]
      }
    } orElse versionOpt
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
      isNewFluenceJs,
      wrapWithOption(helpOpt),
      wrapWithOption(versionOpt),
      FluenceOpts.logLevelOpt,
      constantOpts,
      dryOpt,
      scriptOpt,
      noAirValidation
    ).mapN {
      case (
            inputF,
            importsF,
            outputF,
            toAirOp,
            toJs,
            noRelayOp,
            noXorOp,
            isNewFluenceJsOp,
            h,
            v,
            logLevel,
            constants,
            isDryRun,
            isScheduled,
            disableAirValidation
          ) =>
        val toAir = toAirOp || isScheduled
        val noXor = noXorOp || isScheduled
        val noRelay = noRelayOp || isScheduled

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
          LogFormatter.initLogger(Some(logLevel.compiler))
          logger.info(s"Aqua Compiler $versionStr")

          (inputF, outputF, importsF).mapN { (i, o, imp) =>
            i.andThen { input =>
              o.andThen { output =>
                imp.map { imports =>
                  if (output.isEmpty && !isDryRun)
                    Validated
                      .invalidNec(
                        "Output path should be specified ('--output' or '-o'). " +
                          "Or use --dry if you want to check that the code compiles"
                      )
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
                        targetToBackend(target, !isNewFluenceJsOp),
                        bc,
                        disableAirValidation
                      )
                  }
                }
              }
            }
          }.flatMap { results =>
            printErrorsOr[F, F[ValidatedNec[String, Chain[String]]]](
              results,
              { res =>
                res.flatMap {
                  case Validated.Invalid(errs) =>
                    printCommandErrors[F](errs).as {
                      ExitCode.Error
                    }
                  case Validated.Valid(results) =>
                    results.map(OutputPrinter.print _)
                    ExitCode.Success.pure[F]
                }
              }
            )
          }
        }
    }
  }

  def handleCommand(args: List[String]): IO[ExitCode] = {
    val command = Command("aqua", "Aqua Compiler", false)(main[IO](runtime))
    for {
      parseResult: Either[Help, IO[ExitCode]] <- Sync[IO].delay(
        // ambientArgs returns arguments for scala.js under node.js
        command.parse(PlatformApp.ambientArgs getOrElse args, sys.env)
      )
      exitCode <- parseResult.fold(
        { h =>
          NonEmptyChain
            .fromSeq(h.errors)
            .map { errs =>
              printCommandErrors[IO](errs).as {
                ExitCode.Error
              }
            }
            .getOrElse{
              ConsoleEff[IO].print(h).map{_ =>
                // hack to show last string in `help`
                println()
                ExitCode.Success
              }
            }
        },
        identity
      )
    } yield exitCode
  }

  override def run(args: List[String]): IO[ExitCode] = {
    handleCommand(
      args match {
        // Weird ugly hack: in case version flag or help flag is present, ignore other options,
        // be it correct or not
        case _ if args.contains("-v") || args.contains("--version") => "-v" :: Nil
        case _ if args.contains("-h") || args.contains("--help") => "-h" :: Nil
        case _ => args
      }
    )
  }
}
