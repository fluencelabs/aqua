package aqua.ipfs

import aqua.{
  AppOpts,
  AquaIO,
  CliFunc,
  CommandBuilder,
  FluenceOpts,
  LogFormatter,
  LogLevelTransformer,
  PackagePath,
  PlatformOpts,
  RunInfo,
  SubCommandBuilder
}
import aqua.js.{Fluence, PeerConfig}
import aqua.keypair.KeyPairShow.show
import cats.data.{NonEmptyChain, NonEmptyList, Validated, ValidatedNec, ValidatedNel}
import Validated.{invalid, invalidNec, valid, validNec, validNel}
import aqua.builder.IPFSUploader
import aqua.ipfs.js.IpfsApi
import aqua.model.LiteralModel
import aqua.raw.value.LiteralRaw
import aqua.run.{GeneralOptions, RunCommand, RunConfig, RunOpts}
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
      .option[String]("path", "Path to a file", "p", "path")

  def ipfsOpt[F[_]: Async]: Command[F[ValidatedNec[String, Unit]]] =
    CommandBuilder("ipfs", "Work with IPFS on a peer", NonEmptyList.one(upload[F])).command

  // Uploads a file to IPFS
  def upload[F[_]: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.valid(
      "upload",
      "Upload a file to IPFS",
      (GeneralOptions.opt, pathOpt).mapN { (common, path) =>
        RunInfo(
          common,
          CliFunc(UploadFuncName, LiteralRaw.quote(path) :: Nil),
          Option(PackagePath(IpfsAqua))
        )
      }
    )
}
