package aqua

import com.monovore.decline.{Command, Opts}
import scribe.Logging
import scala.concurrent.{ExecutionContext, Future}
import cats.effect.ExitCode
import cats.effect.kernel.{Async}
import cats.Monad
import cats.implicits.catsSyntaxApplicativeId
import cats.Applicative
import aqua.KeyPair
import cats.Applicative.ops.toAllApplicativeOps

object KeyPairOpts extends Logging {

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
            println(s"keypair: ${KeyPairStringify.stringify(keypair)}")
            ExitCode.Success
          )
      )
    }
}
