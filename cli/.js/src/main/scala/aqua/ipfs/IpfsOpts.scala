package aqua.ipfs

import aqua.{AppOpts, LogFormatter, LogLevelTransformer, OptUtils}
import aqua.io.OutputPrinter
import aqua.js.{Fluence, PeerConfig}
import aqua.keypair.KeyPairShow.show
import cats.data.{NonEmptyChain, NonEmptyList, Validated, ValidatedNec, ValidatedNel}
import Validated.{invalid, invalidNec, valid, validNec, validNel}
import aqua.ipfs.js.IpfsApi
import cats.effect.{Concurrent, ExitCode, Resource, Sync}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.effect.kernel.Async
import cats.syntax.show.*
import cats.{Applicative, Monad}
import com.monovore.decline.{Command, Opts}
import fs2.io.file.Files
import scribe.Logging

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js

// Options and commands to work with KeyPairs
object IpfsOpts extends Logging {

  val multiaddrOpt: Opts[String] =
    Opts
      .option[String]("addr", "Relay multiaddress", "a")

  def pathOpt[F[_]: Files: Concurrent]: Opts[String] =
    Opts
      .option[String]("path", "Path to file", "p")

  def upload[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "upload",
      header = "Upload a file to IPFS"
    ) {
      (pathOpt, multiaddrOpt, AppOpts.logLevelOpt).mapN { (path, multiaddr, logLevel) =>
        LogFormatter.initLogger(Some(logLevel))
        val resource = Resource.make(Fluence.getPeer().pure[F]) { peer =>
          Async[F].fromFuture(Sync[F].delay(peer.stop().toFuture))
        }
        resource.use { peer =>
          Async[F].fromFuture {
            (for {
              _ <- Fluence
                .start(
                  PeerConfig(
                    multiaddr,
                    10000,
                    "debug",
                    null
                  )
                )
                .toFuture
              cid <- IpfsApi
                .uploadFile(
                  path,
                  peer,
                  logger.info: Function1[String, Unit],
                  logger.error: Function1[String, Unit]
                )
                .toFuture
            } yield {
              ExitCode.Success
            }).pure[F]
          }
        }

      }
    }
}
