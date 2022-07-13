package aqua

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import com.monovore.decline.Opts
import scribe.Level
import cats.syntax.traverse.*
import cats.data.Validated.{invalid, invalidNec, invalidNel, valid, validNec, validNel}

import java.util.Base64

case class LogLevels(
  compiler: Level = Level.Error,
  fluencejs: Level = Level.Error,
  aquavm: Level = Level.Fatal
)

object LogLevels {
  def apply(level: Level): LogLevels = LogLevels(level, level, level)

  private def levelFromString(s: String): ValidatedNel[String, Level] = {
    LogLevel.stringToLogLevel
      .get(s.toLowerCase)
      .map(validNel)
      .getOrElse(
        invalidNel(
          s"Unknown log-level '$s'. Please use one of these: 'all', 'trace', 'debug', 'info', 'warn', 'error', 'off'"
        )
      )
  }

  lazy val error =
    "Invalid log-level format. Must be: '<log-level>' or 'compiler=<log-level>,fluencejs=<log-level>,aquavm=<log-level>', where <log-level> is one of these strings: 'all', 'trace', 'debug', 'info', 'warn', 'error', 'off'"

  private def fromStrings(
    name: String,
    level: String,
    logLevels: LogLevels
  ): Validated[NonEmptyList[String], LogLevels] = {
    levelFromString(level).andThen { level =>
      name match {
        case "compiler" =>
          validNel(logLevels.copy(compiler = level))
        case "fluencejs" =>
          validNel(logLevels.copy(fluencejs = level))
        case "aquavm" =>
          validNel(logLevels.copy(aquavm = level))
        case s =>
          invalidNel[String, LogLevels](
            s"Unknown component '$s' in log-level. Please use one of these: 'aquavm', 'compiler' and 'fluencejs'"
          )
      }
    }
  }

  // Format: '<log-level>' or 'compiler=<log-level>,fluencejs=<log-level>,aquavm=<log-level>',
  // where <log-level> is one of these strings: 'all', 'trace', 'debug', 'info', 'warn', 'error', 'off'
  def fromString(s: String): ValidatedNel[String, LogLevels] = {
    s.split(",").toList match {
      case l :: Nil =>
        l.split("=").toList match {
          case n :: ll :: Nil => fromStrings(n, ll, LogLevels())
          case ll :: Nil => levelFromString(ll).map(apply)
          case _ => invalidNel(error)
        }

      case arr =>
        arr.foldLeft(validNel[String, LogLevels](LogLevels())) { case (logLevelV, ss) =>
          logLevelV.andThen { logLevels =>
            ss.split("=").toList match {
              case n :: ll :: Nil => fromStrings(n, ll, logLevels)
              case n :: Nil => levelFromString(n).map(apply)
              case _ => invalidNel[String, LogLevels](error)
            }
          }
        }

    }
  }
}

object FluenceOpts {

  val timeoutOpt: Opts[Int] =
    Opts
      .option[Int]("timeout", "Request timeout in milliseconds", "t")

  val onOpt: Opts[Option[String]] =
    AppOpts.wrapWithOption(
      Opts
        .option[String](
          "on",
          "peerId of the peer that will execute the function. Default: host_peer_id",
          "o",
          "peerId"
        )
    )

  val showConfigOpt: Opts[Boolean] =
    Opts
      .flag("show-config", "Print current configuration on start")
      .map(_ => true)
      .withDefault(false)

  val verboseOpt: Opts[Boolean] =
    Opts
      .flag("verbose", "Show additional information about the call")
      .map(_ => true)
      .withDefault(false)

  val secretKeyOpt: Opts[Array[Byte]] =
    Opts
      .option[String]("sk", "Ed25519 32-byte secret key in base64", "s", "base64")
      .mapValidated { s =>
        val decoder = Base64.getDecoder
        Validated.catchNonFatal {
          decoder.decode(s)
        }.leftMap(t => NonEmptyList.one("secret key isn't a valid base64 string: " + t.getMessage))
      }

  val printAir: Opts[Boolean] =
    Opts
      .flag("print-air", "Prints generated AIR code before function execution")
      .map(_ => true)
      .withDefault(false)

  val logLevelOpt: Opts[LogLevels] =
    Opts.option[String]("log-level", help = "Set log level").mapValidated {
      str =>
        LogLevels.fromString(str)
    }.withDefault(LogLevels())
}
