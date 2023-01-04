package aqua

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import aqua.logging.LogLevels
import com.monovore.decline.Opts
import scribe.Level
import cats.syntax.traverse.*
import cats.data.Validated.{invalid, invalidNec, invalidNel, valid, validNec, validNel}

import java.util.Base64
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration

object FluenceOpts {

  val timeoutOpt: Opts[Duration] =
    Opts
      .option[Int]("timeout", "Request timeout in milliseconds", "t")
      .map(i => Duration(i, TimeUnit.MILLISECONDS))

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
    Opts.option[String]("log-level", help = s"Set log level. ${LogLevels.logHelpMessage}").mapValidated {
      str =>
        LogLevels.fromString(str)
    }.withDefault(LogLevels())
}
