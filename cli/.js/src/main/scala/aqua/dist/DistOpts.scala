package aqua.dist

import aqua.builder.{ArgumentGetter, IPFSUploader}
import aqua.files.AquaFilesIO
import aqua.io.OutputPrinter
import aqua.ipfs.js.IpfsApi
import aqua.js.{Config, Fluence, PeerConfig}
import aqua.keypair.KeyPairShow.show
import aqua.model.LiteralModel
import aqua.raw.value.{LiteralRaw, VarRaw}
import aqua.run.RunCommand.createKeyPair
import aqua.run.{GeneralRunOptions, RunCommand, RunConfig, RunOpts}
import aqua.*
import aqua.run.RunOpts.logger
import aqua.types.ScalarType
import cats.data.Validated.{invalid, invalidNec, valid, validNec, validNel}
import cats.data.*
import cats.effect.kernel.Async
import cats.effect.{Concurrent, ExitCode, Resource, Sync}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.show.*
import cats.{Applicative, Monad}
import com.monovore.decline.{Command, Opts}
import fs2.io.file.{Files, Path}
import scribe.Logging

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js

// Options and commands to work with IPFS
object DistOpts extends Logging {

  val DistAqua = "aqua/dist.aqua"

  val DeployFuncName = "deploy"
  val RemoveFuncName = "remove"

  def srvNameOpt: Opts[String] =
    Opts
      .option[String]("service", "What service from the config file to deploy", "s")

  def srvIdOpt: Opts[String] =
    Opts
      .option[String]("id", "Service id to remove", "i")

  def deployOpt[F[_]: Async]: Command[F[ExitCode]] =
    CommandBuilder(
      "dist",
      "Distribute a service onto a remote peer",
      NonEmptyList(deploy, remove :: Nil)
    ).command

  def fillConfigOptionalFields(getter: ArgumentGetter): ArgumentGetter = {
    val arg = getter.function.arg
    val filledConfig = Config.fillWithEmptyArrays(arg)
    ArgumentGetter(getter.function.value, filledConfig)
  }

  // Removes service from a node
  def remove[F[_]: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.valid(
      "remove",
      "Remove a service from a remote peer",
      (GeneralRunOptions.commonOpt, srvIdOpt).mapN { (common, srvId) =>
        RunInfo(
          common,
          RemoveFuncName,
          PackagePath(DistAqua),
          Nil,
          LiteralRaw.quote(srvId) :: Nil
        )
      }
    )

  // Uploads a file to IPFS, creates blueprints and deploys a service
  def deploy[F[_]: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.applyF(
      "deploy",
      "Deploy a service onto a remote peer",
      (
        GeneralRunOptions.commonOpt,
        ArgOpts.dataFromFileOpt[F],
        srvNameOpt
      ).mapN { (common, dataFromFileF, srvName) =>
        dataFromFileF.map { dff =>
          val args = LiteralRaw.quote(srvName) :: VarRaw(srvName, ScalarType.string) :: Nil
          dff
            .andThen(data =>
              ArgOpts
                .checkDataGetServices(args, Some(data))
                .map(getServices =>
                  // if we have default timeout, increase it
                  val commonWithTimeout = if (common.timeout.isEmpty) {
                    common.copy(timeout = Some(60000))
                  } else common
                  RunInfo(
                    commonWithTimeout,
                    DeployFuncName,
                    PackagePath(DistAqua),
                    Nil,
                    args,
                    // hack: air cannot use undefined fields, fill undefined arrays with nils
                    getServices.map { (k, v) => (k, fillConfigOptionalFields(v)) },
                    Nil
                  )
                )
            )
        }
      }
    )
}
