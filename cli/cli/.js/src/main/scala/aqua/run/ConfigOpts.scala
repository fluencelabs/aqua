package aqua.run

import aqua.AppOpts
import aqua.FluenceOpts.*
import aqua.builder.{ArgumentGetter, Service}
import aqua.config.ConfigOpts.{Krasnodar, Stage, TestNet}
import aqua.js.FluenceEnvironment
import aqua.raw.ConstantRaw
import aqua.raw.value.VarRaw
import aqua.logging.LogLevels
import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{invalidNel, validNel}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import com.monovore.decline.Opts
import scribe.Level

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.scalajs.js
import scala.util.Try

object GeneralOpts {

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
    withConstants: Boolean,
    defaultTimeout: Duration = Duration(7000, TimeUnit.MILLISECONDS)
  ): Opts[GeneralOptions] =
    (
      timeoutOpt.withDefault(defaultTimeout),
      logLevelOpt,
      multiaddrOpt,
      onOpt,
      flagsOpt(isRun),
      if (withSecret) { secretKeyOpt.map(Some.apply) }
      else { AppOpts.wrapWithOption(secretKeyOpt) },
      if (withConstants) AppOpts.constantOpts else Nil.pure[Opts]
    ).mapN(GeneralOptions.apply)

  val opt: Opts[GeneralOptions] = commonOpt(false, false, false)
  val runOpt: Opts[GeneralOptions] = commonOpt(true, false, true)
  val optWithSecretKey: Opts[GeneralOptions] = commonOpt(false, true, false)

  def optWithSecretKeyCustomTimeout(timeoutMs: Int): Opts[GeneralOptions] =
    commonOpt(false, true, false, Duration(timeoutMs, TimeUnit.MILLISECONDS))
}
