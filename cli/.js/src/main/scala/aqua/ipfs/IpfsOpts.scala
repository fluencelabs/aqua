package aqua.ipfs

import aqua.{LogLevelTransformer, OptUtils}
import aqua.io.OutputPrinter
import aqua.js.{Fluence, PeerConfig}
import aqua.keypair.KeyPairShow.show
import cats.data.{NonEmptyChain, NonEmptyList, Validated, ValidatedNec, ValidatedNel}
import Validated.{invalid, invalidNec, valid, validNec, validNel}
import aqua.ipfs.js.IpfsApi
import cats.effect.{Concurrent, ExitCode}
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

  def pathOpt[F[_]: Files: Concurrent]: Opts[F[ValidatedNec[String, Vector[Byte]]]] =
    Opts
      .option[String]("path", "Path to file", "p")
      .map { str =>
        OptUtils.transformPath(
          str,
          p => {
            Files[F]
              .readAll(p)
              .compile
              .fold(Vector.empty[Byte])((acc, b) => acc :+ b)
              .map(res => validNec(res))

          }
        )
      }

  def upload[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "upload",
      header = "Upload a file to IPFS"
    ) {
      (pathOpt, multiaddrOpt).mapN((dataF, multiaddr) =>
        for {
          dataV <- dataF
          resultV: Validated[NonEmptyChain[String], F[Boolean]] = dataV.map { data =>
            Ipfs.upload(data, multiaddr)
          }
        } yield ExitCode.Success
      )
    }
}
