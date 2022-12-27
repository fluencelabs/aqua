package aqua.keypair

import aqua.io.OutputPrinter
import aqua.js.KeyPair
import aqua.keypair.KeyPairShow.show
import cats.data.ValidatedNec
import cats.effect.ExitCode
import cats.effect.kernel.Async
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.show.*
import cats.{Applicative, Monad}
import com.monovore.decline.{Command, Opts}
import cats.data.Validated.{invalidNec, validNec}
import scribe.Logging

import scala.concurrent.{ExecutionContext, Future}

// Options and commands to work with KeyPairs
object KeyPairOpts extends Logging {

  def command[F[_]: Async]: Command[F[ValidatedNec[String, Unit]]] =
    Command(name = "key", header = "Manage local keys and identity") {
      Opts.subcommands(
        createKeypair
      )
    }

  // KeyPair generation
  def createKeypair[F[_]: Async]: Command[F[ValidatedNec[String, Unit]]] =
    Command(
      name = "create",
      header = "Generate new key pair"
    ) {
      Opts.unit.map(_ =>
        Async[F]
          .fromFuture(
            KeyPair.randomEd25519().toFuture.pure[F]
          )
          .map(keypair => validNec(OutputPrinter.print(s"${keypair.show}")))
      )
    }
}
