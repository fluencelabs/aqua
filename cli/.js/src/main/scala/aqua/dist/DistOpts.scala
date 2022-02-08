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
      .option[String]("id", "Service id to delete", "i")

  def deployOpt[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "dist",
      header = "Distribute a service onto a remote peer"
    ) {
      Opts.subcommand(deploy).orElse(Opts.subcommand(remove))
    }

  def fillConfigOptionalFields(getter: ArgumentGetter): ArgumentGetter = {
    val arg = getter.function.arg
    val filledConfig = Config.fillWithEmptyArrays(arg)
    ArgumentGetter(getter.function.value, filledConfig)
  }

  def remove[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "remove",
      header = "Remove a service onto a remote peer"
    ) {
      (
        GeneralRunOptions.commonOptWithSecretKey,
        srvIdOpt
      ).mapN { (common, srvId) =>
        PlatformOpts.getPackagePath(DistAqua).flatMap { distAquaPath =>
          val args = LiteralRaw.quote(srvId) :: Nil
          RunOpts.execRun(
            common,
            RemoveFuncName,
            distAquaPath,
            Nil,
            args
          )
        }
      }
    }

  // Uploads a file to IPFS
  def deploy[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "deploy",
      header = "Deploy a service onto a remote peer"
    ) {
      (
        GeneralRunOptions.commonOpt,
        ArgOpts.dataFromFileOpt[F],
        srvNameOpt
      ).mapN { (common, dataFromFileF, srvName) =>
        dataFromFileF.flatMap { dff =>
          PlatformOpts.getPackagePath(DistAqua).flatMap { distAquaPath =>
            val args = VarRaw(srvName, ScalarType.string) :: Nil
            dff
              .andThen(data =>
                ArgOpts
                  .checkDataGetServices(args, Some(data))
                  .map(getServices =>
                    // if we have default timeout, increase it
                    val commonWithTimeout = if (common.timeout.isEmpty) {
                      common.copy(timeout = Some(60000))
                    } else common
                    RunOpts.execRun(
                      commonWithTimeout,
                      DeployFuncName,
                      distAquaPath,
                      Nil,
                      args,
                      getServices.map { (k, v) => (k, fillConfigOptionalFields(v)) },
                      Nil
                    )
                  )
              )
              .fold(
                errs =>
                  Async[F].pure {
                    errs.map(logger.error)
                    cats.effect.ExitCode.Error
                  },
                identity
              )
          }
        }

      }
    }
}
