package aqua.logging

import cats.syntax.option.*
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.data.Validated.{invalidNel, validNel}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import scribe.Level

case class LogLevels(
  compiler: Level = Level.Error,
  fluencejs: Level = Level.Error,
  aquavm: Level = Level.Fatal
)

object LogLevels {

  val logHelpMessage =
    "Format: '<level> OR <segment>=<level>[,]', where <level> is one of these strings: 'all', 'trace', 'debug', 'info', 'warn', 'error', 'off'. <segment> can be 'compiler', 'fluencejs' or 'aquavm'"

  def apply(level: Level): LogLevels = LogLevels(level, level, level)

  def levelFromString(s: String): ValidatedNel[String, Level] =
    LogLevel.stringToLogLevel
      .get(s.toLowerCase.trim())
      .toValidNel(s"Invalid log-level '$s'. $logHelpMessage")

  lazy val error =
    s"Invalid log-level format. $logHelpMessage"

  private def fromStrings(
    name: String,
    level: String,
    logLevels: LogLevels
  ): Validated[NonEmptyList[String], LogLevels] = {
    levelFromString(level).andThen { level =>
      name.trim().toLowerCase() match {
        case "compiler" =>
          validNel(logLevels.copy(compiler = level))
        case "fluencejs" =>
          validNel(logLevels.copy(fluencejs = level))
        case "aquavm" =>
          validNel(logLevels.copy(aquavm = level))
        case s =>
          invalidNel(
            s"Unknown component '$s' in log-level. Please use one of these: 'aquavm', 'compiler' and 'fluencejs'"
          )
      }
    }
  }

  // Format: '<log-level>' or 'compiler=<log-level>,fluencejs=<log-level>,aquavm=<log-level>',
  // where <log-level> is one of these strings: 'all', 'trace', 'debug', 'info', 'warn', 'error', 'off'
  def fromString(s: String): ValidatedNel[String, LogLevels] =
    s.split(",")
      .toList
      .foldLeftM(LogLevels()) { case (levels, level) =>
        level.split("=").toList match {
          case n :: l :: Nil => fromStrings(n, l, levels).toEither
          case l :: Nil => levelFromString(l).map(apply).toEither
          case _ => invalidNel(error).toEither
        }
      }
      .toValidated
}
