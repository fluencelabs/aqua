package aqua.run

import aqua.FluenceOpts.{logLevelOpt, multiaddrOpt, onOpt, printAir, secretKeyOpt, timeoutOpt}
import aqua.builder.{ArgumentGetter, Service}
import aqua.AppOpts
import com.monovore.decline.Opts
import scribe.Level
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.apply.*

case class GeneralRunOptions(
  timeout: Int,
  logLevel: Level,
  multiaddr: String,
  on: Option[String],
  printAir: Boolean,
  secretKey: Option[Array[Byte]]
)

object GeneralRunOptions {

  val commonOpt: Opts[GeneralRunOptions] =
    (timeoutOpt, logLevelOpt, multiaddrOpt, onOpt, printAir, AppOpts.wrapWithOption(secretKeyOpt))
      .mapN(GeneralRunOptions.apply)
}

// `run` command configuration
case class RunConfig(
  common: GeneralRunOptions,
  // services that will pass arguments to air
  argumentGetters: Map[String, ArgumentGetter],
  // builtin services for aqua run, for example: Console, FileSystem, etc
  services: List[Service],
  resultPrinterServiceId: String = "--after-callback-srv-service--",
  resultPrinterName: String = "console-log",
  finisherServiceId: String = "--finisher--",
  finisherFnName: String = "--finish-execution--",
  resultName: String = "-some-unique-res-name-",
  functionWrapperName: String = "--someFuncToRun--"
)
