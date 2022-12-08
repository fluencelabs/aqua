package aqua.logging

import scribe.Level
import cats.data.Validated.{invalid, invalidNec, invalidNel, valid, validNec, validNel}
import cats.data.{Validated, ValidatedNel}
import cats.data.NonEmptyList

case class LogLevels(
  compiler: Level = Level.Error,
  fluencejs: Level = Level.Error,
  aquavm: Level = Level.Fatal
)

object LogLevels {

  val logHelpMessage = "Format: '<level> OR <segment>=<level>[,]', where <level> is one of these strings: 'all', 'trace', 'debug', 'info', 'warn', 'error', 'off'. <segment> can be 'compiler', 'fluencejs' or 'aquavm'"

  def apply(level: Level): LogLevels = LogLevels(level, level, level)

  private def levelFromString(s: String): ValidatedNel[String, Level] = {
    LogLevel.stringToLogLevel
      .get(s.toLowerCase)
      .map(validNel)
      .getOrElse(
        invalidNel(
          s"Invalid log-level '$s'. $logHelpMessage"
        )
      )
  }

  lazy val error =
    s"Invalid log-level format. $logHelpMessage"

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
