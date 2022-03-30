package aqua.run

import aqua.FluenceOpts.{logLevelOpt, onOpt, printAir, secretKeyOpt, showConfigOpt, timeoutOpt}
import aqua.builder.{ArgumentGetter, Service}
import aqua.AppOpts
import aqua.config.ConfigOpts.{Krasnodar, Stage, TestNet}
import aqua.js.FluenceEnvironment
import com.monovore.decline.Opts
import scribe.Level
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.data.Validated
import cats.data.Validated.{invalidNel, validNel}
import cats.data.NonEmptyList

import scala.util.Try

case class GeneralRunOptions(
  timeout: Option[Int],
  logLevel: Level,
  multiaddr: String,
  on: Option[String],
  printAir: Boolean,
  secretKey: Option[Array[Byte]],
  showConfig: Boolean
)

object GeneralRunOptions {

  val multiaddrOpt: Opts[String] =
    Opts
      .option[String]("addr", "Relay multiaddress", "a")
      .mapValidated { s =>
        if ((s.startsWith("/dns4/") || s.startsWith("/ip4/")) && s.contains("/p2p/12D3")) {
          validNel(s)
        } else {
          Validated.catchNonFatal {
            val splitted = s.split("-")
            val index = splitted(1).toInt
            splitted.head.toLowerCase match {
              case Krasnodar =>
                validNel(FluenceEnvironment.krasnodar(index).multiaddr)
              case TestNet =>
                validNel(FluenceEnvironment.testnet(index).multiaddr)
              case Stage =>
                validNel(FluenceEnvironment.stage(index).multiaddr)
              case _ =>
                invalidNel("Invalid multiaddr format.")
            }
          }.andThen(identity)
            .leftMap(_ =>
              NonEmptyList.one(
                "Invalid multiaddr format. Run 'aqua config default_peers' for valid multiaddress."
              )
            )
        }
      }

  val commonOpt: Opts[GeneralRunOptions] =
    (
      AppOpts.wrapWithOption(timeoutOpt),
      logLevelOpt,
      multiaddrOpt,
      onOpt,
      printAir,
      AppOpts.wrapWithOption(secretKeyOpt),
      showConfigOpt
    )
      .mapN(GeneralRunOptions.apply)

  val commonOptWithSecretKey: Opts[GeneralRunOptions] =
    (
      AppOpts.wrapWithOption(timeoutOpt),
      logLevelOpt,
      multiaddrOpt,
      onOpt,
      printAir,
      secretKeyOpt.map(Some.apply),
      showConfigOpt
    )
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
