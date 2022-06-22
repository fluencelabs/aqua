package aqua.run

import aqua.{AppOpts, VarJson}
import aqua.FluenceOpts.*
import aqua.builder.{ArgumentGetter, Service}
import aqua.config.ConfigOpts.{Krasnodar, Stage, TestNet}
import aqua.js.FluenceEnvironment
import aqua.raw.ConstantRaw
import aqua.raw.value.VarRaw
import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{invalidNel, validNel}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import com.monovore.decline.Opts
import scribe.Level

import scala.scalajs.js
import scala.util.Try

case class Flags(
  printAir: Boolean,
  showConfig: Boolean,
  verbose: Boolean,
  noXor: Boolean,
  noRelay: Boolean
)

case class GeneralRunOptions(
  timeout: Option[Int],
  logLevel: Level,
  multiaddr: String,
  on: Option[String],
  flags: Flags,
  secretKey: Option[Array[Byte]],
  constants: List[ConstantRaw]
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
                invalidNel(
                  // TODO: maybe show an example of valid format in this error message and in the one below
                  "Invalid multiaddr format. Run 'aqua config default_peers' for valid multiaddress."
                )
            }
          }.andThen(identity)
            .leftMap(_ =>
              NonEmptyList.one(
                "Invalid multiaddr format. Run 'aqua config default_peers' for valid multiaddress."
              )
            )
        }
      }

  def flagsOpt(isRun: Boolean): Opts[Flags] =
    ((
      printAir,
      showConfigOpt,
      verboseOpt
    ) ++ {
      if (isRun)
        (AppOpts.noXorWrapper, AppOpts.noRelay)
      else
        (false.pure[Opts], false.pure[Opts])
    }).mapN(Flags.apply)

  def commonOpt(
    isRun: Boolean,
    withSecret: Boolean,
    withConstants: Boolean
  ): Opts[GeneralRunOptions] =
    (
      AppOpts.wrapWithOption(timeoutOpt),
      logLevelOpt.withDefault(Level.Fatal),
      multiaddrOpt,
      onOpt,
      flagsOpt(isRun),
      if (withSecret) { secretKeyOpt.map(Some.apply) }
      else { AppOpts.wrapWithOption(secretKeyOpt) },
      if (withConstants) AppOpts.constantOpts else Nil.pure[Opts]
    ).mapN(GeneralRunOptions.apply)

  val commonGeneralOpt: Opts[GeneralRunOptions] = commonOpt(false, false, false)
  val commonGeneralRunOpt: Opts[GeneralRunOptions] = commonOpt(true, false, true)
  val commonGeneralOptWithSecretKey: Opts[GeneralRunOptions] = commonOpt(false, true, false)
}

// `run` command configuration
case class RunConfig(
  common: GeneralRunOptions,
  // services that will pass arguments to air
  argumentGetters: Map[String, VarJson],
  // builtin services for aqua run, for example: Console, FileSystem, etc
  services: List[Service],
  resultPrinterServiceId: String = "--after-callback-srv-service--",
  resultPrinterName: String = "console-log",
  finisherServiceId: String = "--finisher--",
  finisherFnName: String = "--finish-execution--",
  resultName: String = "-some-unique-res-name-",
  functionWrapperName: String = "--someFuncToRun--"
)
