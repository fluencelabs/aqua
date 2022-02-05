package aqua.ipfs

import aqua.{AppOpts, AquaIO, FluenceOpts, LogFormatter, LogLevelTransformer, PlatformOpts}
import aqua.io.OutputPrinter
import aqua.js.{Fluence, PeerConfig}
import aqua.keypair.KeyPairShow.show
import cats.data.{NonEmptyChain, NonEmptyList, Validated, ValidatedNec, ValidatedNel}
import Validated.{invalid, invalidNec, valid, validNec, validNel}
import aqua.builder.IPFSUploader
import aqua.files.AquaFilesIO
import aqua.ipfs.js.IpfsApi
import aqua.model.LiteralModel
import aqua.raw.value.LiteralRaw
import aqua.run.RunCommand.createKeyPair
import aqua.run.{GeneralRunOptions, RunCommand, RunConfig, RunOpts}
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

  val IpfsAqua = "aqua/ipfs.aqua"

  val UploadFuncName = "uploadFile"

  def pathOpt: Opts[String] =
    Opts
      .option[String]("path", "Path to file", "p")

  def ipfsOpt[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "ipfs",
      header = "Working with IPFS on peer"
    ) { Opts.subcommand(upload) }

  // Uploads a file to IPFS
  def upload[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "upload",
      header = "Upload a file to IPFS"
    ) {
      (
        GeneralRunOptions.commonOpt,
        pathOpt
      ).mapN { (common, path) =>
        PlatformOpts.getPackagePath(IpfsAqua).flatMap { ipfsAquaPath =>
          RunOpts.execRun(
            common,
            UploadFuncName,
            ipfsAquaPath,
            Nil,
            LiteralRaw.quote(path) :: Nil,
            Map.empty,
            Nil
          )
        }

      }
    }
}
