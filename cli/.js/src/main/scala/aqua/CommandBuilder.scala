package aqua

import aqua.builder.{ArgumentGetter, Service}
import aqua.raw.value.ValueRaw
import aqua.run.{GeneralRunOptions, RunOpts}
import com.monovore.decline.{Command, Opts}
import fs2.io.file.{Files, Path}
import cats.effect.ExitCode
import cats.effect.kernel.Async
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.monad.*
import cats.syntax.functor.*
import cats.data.{NonEmptyList, Validated, ValidatedNec}
import Validated.{invalid, invalidNec, valid, validNec, validNel}
import scribe.Logging

import scala.concurrent.ExecutionContext

sealed trait AquaPath {
  val path: String
}
// Path for package relative files
case class PackagePath(path: String) extends AquaPath
// Path for absolute or call path relative files
case class RelativePath(path: String) extends AquaPath

case class RunInfo(
  common: GeneralRunOptions,
  funcName: String,
  input: AquaPath,
  imports: List[Path] = Nil,
  args: List[ValueRaw] = Nil,
  argumentGetters: Map[String, ArgumentGetter] = Map.empty,
  services: List[Service] = Nil
)

// Builds subcommand
class SubCommandBuilder(name: String, header: String, opts: Opts[ValidatedNec[String, RunInfo]])
    extends Logging {

  def command[F[_]: Files: Async](implicit
    ec: ExecutionContext
  ): Command[F[ExitCode]] = Command(name, header) {
    opts.map {
      case Validated.Valid(ri) =>
        (ri.input match {
          case PackagePath(p) => PlatformOpts.getPackagePath(p)
          case RelativePath(p) => Path(p).pure[F]
        }).flatMap { path =>
          RunOpts.execRun(
            ri.common,
            ri.funcName,
            path,
            ri.imports,
            ri.args,
            ri.argumentGetters,
            ri.services
          )
        }
      case Validated.Invalid(errs) =>
        errs.map(logger.error)
        ExitCode.Error.pure[F]

    }
  }
}

object SubCommandBuilder {

  def apply(
    name: String,
    header: String,
    opts: Opts[ValidatedNec[String, RunInfo]]
  ): SubCommandBuilder = {
    new SubCommandBuilder(name, header, opts)
  }

  def valid(name: String, header: String, opts: Opts[RunInfo]): SubCommandBuilder = {
    new SubCommandBuilder(name, header, opts.map(ri => validNec[String, RunInfo](ri)))
  }

  def simple(
    name: String,
    header: String,
    path: AquaPath,
    funcName: String
  ): SubCommandBuilder =
    SubCommandBuilder
      .valid(
        name,
        header,
        GeneralRunOptions.commonOpt.map { c =>
          RunInfo(c, funcName, path)
        }
      )

  def subcommands[F[_]: Files: Async](subs: NonEmptyList[SubCommandBuilder])(implicit
    ec: ExecutionContext
  ): Opts[F[ExitCode]] =
    Opts.subcommands(subs.head.command[F], subs.tail.map(_.command[F]): _*)
}

// Builds top command with subcommands
class CommandBuilder(
  name: String,
  header: String,
  subcommands: NonEmptyList[SubCommandBuilder]
) {

  def command[F[_]: Files: Async](implicit
    ec: ExecutionContext
  ): Command[F[ExitCode]] = {
    Command(name = name, header = header) {
      Opts.subcommands(subcommands.head.command[F], subcommands.tail.map(_.command[F]): _*)
    }
  }
}
