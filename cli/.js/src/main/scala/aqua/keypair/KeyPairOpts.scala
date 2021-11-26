package aqua.keypair

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

// Options and commands to work with KeyPairs
object KeyPairOpts extends Logging {

  // Used to pass existing keypair to AquaRun
  val secretKey: Opts[String] =
    Opts.option[String]("secret-key", "Ed25519 32-byte key in base64", "sk")

  // KeyPair generation
  def createKeypair[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "create_keypair",
      header = "Create a new keypair"
    ) {
      Opts.unit.map(_ =>
        Async[F]
          .fromFuture(
            KeyPair.randomEd25519().toFuture.pure[F]
          )
          .map(keypair =>
            println(s"${KeyPairStringify.stringify(keypair)}")
            ExitCode.Success
          )
      )
    }
}
