/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.logging

import cats.syntax.option.*
import cats.syntax.either.*
import cats.syntax.foldable.*
import cats.syntax.validated.*
import cats.data.Validated.*
import cats.data.{NonEmptyList, Validated, ValidatedNec}
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

  def levelFromString(s: String): ValidatedNec[String, Level] =
    LogLevel.stringToLogLevel
      .get(s.toLowerCase.trim())
      .toValidNec(s"Invalid log-level '$s'. $logHelpMessage")

  lazy val error =
    s"Invalid log-level format. $logHelpMessage"

  private def fromStrings(
    name: String,
    level: String,
    logLevels: LogLevels
  ): ValidatedNec[String, LogLevels] = {
    levelFromString(level).andThen { level =>
      name.trim().toLowerCase() match {
        case "compiler" =>
          logLevels.copy(compiler = level).validNec
        case "fluencejs" =>
          logLevels.copy(fluencejs = level).validNec
        case "aquavm" =>
          logLevels.copy(aquavm = level).validNec
        case s =>
          invalidNec(
            s"Unknown component '$s' in log-level. Please use one of these: 'aquavm', 'compiler' and 'fluencejs'"
          )
      }
    }
  }

  // Format: '<log-level>' or 'compiler=<log-level>,fluencejs=<log-level>,aquavm=<log-level>',
  // where <log-level> is one of these strings: 'all', 'trace', 'debug', 'info', 'warn', 'error', 'off'
  def fromString(s: String): ValidatedNec[String, LogLevels] =
    s.split(",")
      .toList
      .foldLeftM(LogLevels()) { case (levels, level) =>
        level.split("=").toList match {
          case n :: l :: Nil => fromStrings(n, l, levels).toEither
          case l :: Nil => levelFromString(l).map(apply).toEither
          case _ => error.invalidNec.toEither
        }
      }
      .toValidated
}
