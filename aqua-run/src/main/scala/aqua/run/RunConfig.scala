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
