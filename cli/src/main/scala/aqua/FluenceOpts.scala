package aqua

import cats.data.{NonEmptyList, Validated}
import com.monovore.decline.Opts

import java.util.Base64

object FluenceOpts {

  val multiaddrOpt: Opts[String] =
    Opts
      .option[String]("addr", "Relay multiaddress", "a")

  val secretKeyOpt: Opts[Array[Byte]] =
    Opts
      .option[String]("sk", "Ed25519 32-byte secret key in base64", "s")
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
}
