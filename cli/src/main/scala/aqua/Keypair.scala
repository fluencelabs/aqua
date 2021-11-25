package aqua

import com.monovore.decline.{Command, Opts}
import scribe.Logging
import scala.concurrent.{ExecutionContext, Future}
import cats.effect.{ExitCode, IO}
import cats.effect.kernel.{Async}
import cats.Monad
import cats.implicits.catsSyntaxApplicativeId
import cats.Applicative
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import aqua.KeyPair
import cats.Applicative.ops.toAllApplicativeOps
import scala.scalajs.js.JSON
import java.util.Base64

object KeypairOpts extends Logging {

  val secretKey: Opts[String] =
    Opts.option[String]("secret-key", "Ed25519 32-byte key in base64", "sk")

  def opts[F[_]: Async](implicit
    ec: ExecutionContext
  ): Opts[Unit] = Opts.unit

  def createKeypair[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "create_keypair",
      header = "Create a new keypair"
    ) {
      opts.map(_ =>
        Async[F]
          .fromFuture(
            KeyPair.randomEd25519().toFuture.pure[F]
          )
          .map(keypair =>
            val encoder = Base64.getEncoder()
            val kp = js.Dynamic.literal(
                peerId = keypair.Libp2pPeerId.toB58String(),
                secretKey = encoder.encodeToString(keypair.toEd25519PrivateKey().toArray.map(s => s.toByte)),
                publicKey = encoder.encodeToString(keypair.Libp2pPeerId.pubKey.bytes.toArray.map(s => s.toByte)),
            )
            logger.info(s"keypair: ${JSON.stringify(kp, space = 2)}")
            ExitCode.Success
          )
      )
    }
}
