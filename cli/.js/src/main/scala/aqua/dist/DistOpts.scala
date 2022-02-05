package aqua.dist

import aqua.builder.IPFSUploader
import aqua.files.AquaFilesIO
import aqua.io.OutputPrinter
import aqua.ipfs.js.IpfsApi
import aqua.js.{Fluence, PeerConfig}
import aqua.keypair.KeyPairShow.show
import aqua.model.LiteralModel
import aqua.raw.value.{LiteralRaw, VarRaw}
import aqua.run.RunCommand.createKeyPair
import aqua.run.{GeneralRunOptions, RunCommand, RunConfig, RunOpts}
import aqua.*
import aqua.run.RunOpts.{checkDataGetServices, dataFromFileOpt, logger}
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

  val DistAquaPath = "aqua/dist.aqua"
  val DeployFuncName = "deploy"

  def srvNameOpt: Opts[String] =
    Opts
      .option[String]("service", "What service from the config file to deploy", "s")

  def deployOpt[F[_] : Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "dist",
      header = "Distribute a service onto a remote peer"
    ) {
      Opts.subcommand(deploy)
    }

  // Uploads a file to IPFS
  def deploy[F[_] : Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "deploy",
      header = "Deploy a service onto a remote peer"
    ) {
      (
        GeneralRunOptions.commonOpt,
        dataFromFileOpt[F],
        srvNameOpt
        ).mapN { (common, dataFromFileF, srvName) =>
        dataFromFileF.flatMap { dff =>
          val args = VarRaw(srvName, ScalarType.string) :: Nil
          dff
            .andThen(data =>
              checkDataGetServices(args, data).map(getServices =>
                RunOpts.execRun(
                  common,
                  DeployFuncName,
                  Path(DistAquaPath),
                  Nil,
                  args,
                  getServices,
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
