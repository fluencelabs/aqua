package aqua

import cats.Functor
import cats.data.Validated.{Invalid, Valid}
import cats.data.{Validated, ValidatedNel}
import cats.effect.ExitCode
import cats.effect.std.Console
import cats.syntax.functor._
import cats.syntax.traverse._
import com.monovore.decline.Opts.help
import com.monovore.decline.enumeratum._
import com.monovore.decline.{Opts, Visibility}

import java.nio.file.Path
import scala.util.Try

object AppOps {

  val helpOpt: Opts[Unit] =
    Opts.flag("help", help = "Display this help text.", "h", Visibility.Partial).asHelp.as(())

  val versionOpt: Opts[Unit] =
    Opts.flag("version", help = "Show version.", "v", Visibility.Partial)

  val logLevelOpt: Opts[LogLevel] =
    Opts.option[LogLevel]("log-level", help = "Set log level.").withDefault(LogLevel.Info)

  def checkPath: Path => ValidatedNel[String, Path] = { p =>
    Validated
      .fromEither(Try {
        val f = p.toFile
        if (f.exists() && f.isDirectory) {
          Right(p)
        } else {
          Left(s"There is no path ${p.toString} or it is not a directory.")
        }
      }.toEither.left.map(t => s"Error occurred on imports reading: ${t.getMessage}").flatten)
      .toValidatedNel
  }

  val inputOpts: Opts[Path] =
    Opts
      .option[Path](
        "input",
        "Path to the input directory that contains your .aqua files. Could be only as a directory.",
        "i"
      )
      .mapValidated(checkPath)

  val outputOpts: Opts[Path] =
    Opts.option[Path]("output", "Path to the output directory", "o").mapValidated(checkPath)

  val importOpts: Opts[LazyList[Path]] =
    Opts
      .options[Path]("import", "Path to the directory to import from", "m")
      .mapValidated { ps =>
        val checked = ps
          .map(p => {
            Validated.catchNonFatal {
              val f = p.toFile
              if (f.exists() && f.isDirectory) {
                Right(p)
              } else {
                Left(s"There is no path ${p.toString} or it is not a directory.")
              }
            }
          })
          .toList

        checked.map {
          case Validated.Valid(pE) =>
            pE match {
              case Right(p) =>
                Validated.Valid(p)
              case Left(e) =>
                Validated.Invalid(e)
            }
          case Validated.Invalid(e) =>
            Validated.Invalid(s"Error occurred on imports reading: ${e.getMessage}")
        }.traverse {
          case Valid(a) => Validated.validNel(a)
          case Invalid(e) => Validated.invalidNel(e)
        }.map(_.to(LazyList))
      }
      .withDefault(LazyList.empty)

  val compileToAir: Opts[Boolean] =
    Opts
      .flag("air", "Generate .air file instead of typescript", "a")
      .map(_ => true)
      .withDefault(false)

  val noRelay: Opts[Boolean] =
    Opts
      .flag("no-relay", "Do not generate a pass through the relay node")
      .map(_ => true)
      .withDefault(false)

  val noXorWrapper: Opts[Boolean] =
    Opts
      .flag("no-xor", "Do not generate a wrapper that catches and displays errors")
      .map(_ => true)
      .withDefault(false)

  lazy val versionStr: String =
    Option(getClass.getPackage.getImplementationVersion).filter(_.nonEmpty).getOrElse("no version")

  def versionAndExit[F[_]: Console: Functor]: F[ExitCode] = Console[F]
    .println(versionStr)
    .as(ExitCode.Success)

  def helpAndExit[F[_]: Console: Functor]: F[ExitCode] = Console[F]
    .println(help)
    .as(ExitCode.Success)

  def wrapWithOption[A](opt: Opts[A]): Opts[Option[A]] =
    opt.map(v => Some(v)).withDefault(None)
}
