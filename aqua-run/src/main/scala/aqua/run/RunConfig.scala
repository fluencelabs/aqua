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

package aqua.run

import aqua.logging.LogLevels
import aqua.raw.ConstantRaw
import aqua.raw.value.VarRaw
import scribe.Level

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.util.Try

case class Flags(
  printAir: Boolean,
  showConfig: Boolean,
  verbose: Boolean,
  noXor: Boolean,
  noRelay: Boolean
)

case class GeneralOptions(
  timeout: Duration,
  logLevel: LogLevels,
  multiaddr: String,
  on: Option[String],
  flags: Flags,
  secretKey: Option[Array[Byte]],
  constants: List[ConstantRaw]
)

// `run` command configuration
case class RunConfig(
  common: GeneralOptions,
  resultPrinterServiceId: String = "--after-callback-srv-service--",
  resultPrinterName: String = "console-log",
  finisherServiceId: String = "--finisher--",
  finisherFnName: String = "--finish-execution--",
  resultName: String = "-some-unique-res-name-",
  functionWrapperName: String = "--someFuncToRun--"
)
