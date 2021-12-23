package aqua.ipfs

import aqua.{AppOpts, AquaIO, FluenceOpts, LogFormatter, LogLevelTransformer}
import aqua.io.OutputPrinter
import aqua.js.{Fluence, PeerConfig}
import aqua.keypair.KeyPairShow.show
import cats.data.{NonEmptyChain, NonEmptyList, Validated, ValidatedNec, ValidatedNel}
import Validated.{invalid, invalidNec, valid, validNec, validNel}
import aqua.builder.IPFSUploader
import aqua.files.AquaFilesIO
import aqua.ipfs.js.IpfsApi
import aqua.model.LiteralModel
import aqua.run.RunCommand.createKeyPair
import aqua.run.{RunCommand, RunConfig, RunOpts}
import cats.effect.{Concurrent, ExitCode, Resource, Sync}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.effect.kernel.Async
import cats.syntax.show.*
import cats.{Applicative, Monad}
import com.monovore.decline.{Command, Opts}
import fs2.io.file.{Files, Path}
import scribe.Logging

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js

// Options and commands to work with IPFS
object IpfsOpts extends Logging {

  val IpfsAquaPath = "aqua/ipfs.aqua"
  val UploadFuncName = "uploadFile"

  def pathOpt[F[_]: Files: Concurrent]: Opts[String] =
    Opts
      .option[String]("path", "Path to file", "p")

  // Uploads a file to IPFS
  def upload[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "upload",
      header = "Upload a file to IPFS"
    ) {
      (
        pathOpt,
        FluenceOpts.multiaddrOpt,
        AppOpts.logLevelOpt,
        AppOpts.wrapWithOption(FluenceOpts.secretKeyOpt),
        RunOpts.timeoutOpt,
        FluenceOpts.printAir
      ).mapN { (path, multiaddr, logLevel, secretKey, timeout, printAir) =>
        LogFormatter.initLogger(Some(logLevel))
        val ipfsUploader = new IPFSUploader("ipfs", "uploadFile")
        implicit val aio: AquaIO[F] = new AquaFilesIO[F]
        RunCommand
          .run[F](
            multiaddr,
            UploadFuncName,
            LiteralModel.quote(path) :: Nil,
            Path(IpfsAquaPath),
            Nil,
            RunConfig(timeout, logLevel, printAir, secretKey, Map.empty, ipfsUploader :: Nil)
          )
          .map(_ => ExitCode.Success)
      }
    }
}
