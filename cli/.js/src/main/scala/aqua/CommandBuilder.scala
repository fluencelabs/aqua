package aqua

import aqua.builder.{ArgumentGetter, Service}
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.run.{GeneralRunOptions, RunCommand, RunOpts}
import cats.data.Validated.{invalid, invalidNec, valid, validNec, validNel}
import cats.data.{NonEmptyList, Validated, ValidatedNec}
import cats.effect.ExitCode
import cats.effect.kernel.Async
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monad.*
import cats.{Applicative, Monad}
import com.monovore.decline.{Command, Opts}
import fs2.io.file.{Files, Path}
import scribe.Logging

import scalajs.js

import scala.concurrent.ExecutionContext

sealed trait AquaPath
// Path for package relative files
case class PackagePath(path: String) extends AquaPath
// Path for absolute or call path relative files
case class RelativePath(path: Path) extends AquaPath

// All info to run any aqua function
case class RunInfo(
  common: GeneralRunOptions,
  func: CliFunc,
  input: AquaPath,
  imports: List[Path] = Nil,
  argumentGetters: Map[String, (VarRaw, js.Dynamic)] = Map.empty,
  services: List[Service] = Nil
)

// Builds subcommand
class SubCommandBuilder[F[_]: Async](
  name: String,
  header: String,
  opts: Opts[F[ValidatedNec[String, RunInfo]]]
) extends Logging {

  def command: Command[F[ValidatedNec[String, Unit]]] = Command(name, header) {
    opts.map { riF =>
      riF.flatMap {
        case Validated.Valid(ri) =>
          LogFormatter.initLogger(Some(ri.common.logLevel))
          (ri.input match {
            case PackagePath(p) => PlatformOpts.getPackagePath(p)
            case RelativePath(p) => p.pure[F]
          }).flatMap { path =>
            RunCommand.execRun(
              ri.common,
              ri.func,
              path,
              ri.imports,
              ri.argumentGetters,
              ri.services
            )
          }
        case i @ Validated.Invalid(_) =>
          i.pure[F]
      }
    }
  }
}

object SubCommandBuilder {

  def apply[F[_]: Async](
    name: String,
    header: String,
    opts: Opts[ValidatedNec[String, RunInfo]]
  ): SubCommandBuilder[F] = {
    new SubCommandBuilder(name, header, opts.map(_.pure[F]))
  }

  def applyF[F[_]: Async](
    name: String,
    header: String,
    opts: Opts[F[ValidatedNec[String, RunInfo]]]
  ): SubCommandBuilder[F] = {
    new SubCommandBuilder(name, header, opts)
  }

  def valid[F[_]: Async](
    name: String,
    header: String,
    opts: Opts[RunInfo]
  ): SubCommandBuilder[F] = {
    SubCommandBuilder(name, header, opts.map(ri => validNec[String, RunInfo](ri)))
  }

  def simple[F[_]: Async](
    name: String,
    header: String,
    path: AquaPath,
    funcName: String
  ): SubCommandBuilder[F] =
    SubCommandBuilder
      .valid(
        name,
        header,
        GeneralRunOptions.commonGeneralOpt.map { c =>
          RunInfo(c, CliFunc(funcName), path)
        }
      )

  def subcommands[F[_]: Async](
    subs: NonEmptyList[SubCommandBuilder[F]]
  ): Opts[F[ValidatedNec[String, Unit]]] =
    Opts.subcommands(subs.head.command, subs.tail.map(_.command): _*)
}

// Builds top command with subcommands
case class CommandBuilder[F[_]: Async](
  name: String,
  header: String,
  subcommands: NonEmptyList[SubCommandBuilder[F]],
  rawCommands: List[Command[F[ValidatedNec[String, Unit]]]] = Nil
) {

  def command: Command[F[ValidatedNec[String, Unit]]] = {
    Command(name = name, header = header) {
      Opts.subcommands(
        subcommands.head.command,
        (subcommands.tail.map(_.command) ++ rawCommands): _*
      )
    }
  }
}
