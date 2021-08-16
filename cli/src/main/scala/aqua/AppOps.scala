package aqua

import aqua.backend.Version
import aqua.model.LiteralModel
import aqua.model.transform.GenerationConfig
import aqua.parser.expr.ConstantExpr
import aqua.parser.lift.LiftParser
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated, ValidatedNec, ValidatedNel}
import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.{Comonad, Functor, Monad}
import com.monovore.decline.Opts.help
import com.monovore.decline.{Opts, Visibility}
import scribe.Level
import fs2.io.file.{Files, Path}

object AppOps {

  val helpOpt: Opts[Unit] =
    Opts.flag("help", help = "Display this help text", "h", Visibility.Partial).asHelp.as(())

  val versionOpt: Opts[Unit] =
    Opts.flag("version", help = "Show version", "v", Visibility.Partial)

  val logLevelOpt: Opts[Level] =
    Opts.option[String]("log-level", help = "Set log level").withDefault("info").mapValidated {
      str =>
        Validated.fromEither(toLogLevel(str))
    }

  def toLogLevel(logLevel: String): Either[NonEmptyList[String], Level] = {
    LogLevel.stringToLogLevel
      .get(logLevel.toLowerCase)
      .toRight(
        NonEmptyList(
          "log-level could be only 'all', 'trace', 'debug', 'info', 'warn', 'error', 'off'",
          Nil
        )
      )
  }

  def checkPath[F[_]: Monad: Files](pathStr: String): F[ValidatedNec[String, Path]] = {
    val p = Path(pathStr)
    Files[F]
      .exists(p)
      .flatMap { exists =>
        if (exists)
          Files[F].isRegularFile(p).map { isFile =>
            if (isFile) {
              val filename = p.fileName.toString
              val ext = Option(filename)
                .filter(_.contains("."))
                .map(f => f.substring(f.lastIndexOf(".") + 1))
                .getOrElse("")
              if (ext != "aqua") Validated.invalidNec("File must be with 'aqua' extension")
              else Validated.validNec(p)
            } else
              Validated.validNec(p)
          }
        else
          Validated.invalidNec(s"There is no path '${p.toString}'").pure[F]
      }
  }

  def inputOpts[F[_]: Monad: Files]: Opts[F[ValidatedNec[String, Path]]] =
    Opts
      .option[String](
        "input",
        "Path to an aqua file or an input directory that contains your .aqua files",
        "i"
      )
      .map(s => checkPath[F](s))

  def outputOpts[F[_]: Monad: Files]: Opts[F[ValidatedNec[String, Path]]] =
    Opts.option[String]("output", "Path to the output directory", "o").map(s => checkPath[F](s))

  def importOpts[F[_]: Monad: Files]: Opts[F[ValidatedNec[String, List[Path]]]] =
    Opts
      .options[String]("import", "Path to the directory to import from", "m")
      .map { ps =>
        val checked: List[F[ValidatedNec[String, Path]]] = ps.toList.map { pStr =>
          val p = Path(pStr)
          (for {
            exists <- Files[F].exists(p)
            isDir <- Files[F].isDirectory(p)
          } yield {
            if (exists && isDir) Validated.validNec[String, Path](p)
            else
              Validated.invalidNec[String, Path](
                s"There is no path ${p.toString} or it is not a directory"
              )
          })
        }

        checked.sequence.map(_.sequence)
      }

  def constantOpts[F[_]: LiftParser: Comonad]: Opts[List[GenerationConfig.Const]] =
    Opts
      .options[String]("const", "Constant that will be used in an aqua code", "c")
      .mapValidated { strs =>
        val parsed = strs.map(s => ConstantExpr.onlyLiteral.parseAll(s))

        val errors = parsed.collect { case Left(er) =>
          er
        }

        NonEmptyList
          .fromList(errors)
          .fold(
            Validated.validNel[String, List[GenerationConfig.Const]](parsed.collect {
              case Right(v) =>
                GenerationConfig.Const(v._1.value, LiteralModel(v._2.value, v._2.ts))
            })
          ) { errors =>
            Validated.invalid(errors.map(_.toString))
          }
      }
      .withDefault(List.empty)

  val compileToAir: Opts[Boolean] =
    Opts
      .flag("air", "Generate .air file instead of typescript", "a")
      .map(_ => true)
      .withDefault(false)

  val compileToJs: Opts[Boolean] =
    Opts
      .flag("js", "Generate .js file instead of typescript")
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
    Version.version

  def versionAndExit[F[_]: Console: Functor]: F[ExitCode] = Console[F]
    .println(versionStr)
    .as(ExitCode.Success)

  def helpAndExit[F[_]: Console: Functor]: F[ExitCode] = Console[F]
    .println(help)
    .as(ExitCode.Success)

  def wrapWithOption[A](opt: Opts[A]): Opts[Option[A]] =
    opt.map(v => Some(v)).withDefault(None)
}
