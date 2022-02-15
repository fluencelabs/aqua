package aqua.network

import aqua.{AppOpts, AquaIO, FluenceOpts, LogFormatter}
import aqua.builder.IPFSUploader
import aqua.files.AquaFilesIO
import aqua.ipfs.IpfsOpts.{pathOpt, UploadFuncName}
import aqua.model.{LiteralModel, ValueModel}
import aqua.raw.value.{LiteralRaw, ValueRaw}
import aqua.run.{GeneralRunOptions, RunCommand, RunConfig, RunOpts}
import aqua.PlatformOpts
import aqua.js.FluenceEnvironment
import cats.effect.ExitCode
import cats.effect.kernel.Async
import cats.Applicative
import com.monovore.decline.{Command, Opts}
import fs2.io.file.Path
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.apply.*

import scala.concurrent.ExecutionContext
import scala.scalajs.js

object NetworkOpts {

  val NetworkAqua = "aqua/network-info.aqua"

  val ListModulesFuncName = "list_modules"
  val ListBlueprintsFuncName = "list_blueprints"
  val ListInterfacesByPeerFuncName = "list_interfaces_by_peer"
  val ListInterfacesFuncName = "list_services"
  val GetInterfaceFuncName = "get_interface"
  val GetModuleInterfaceFuncName = "get_module_interface"

  val Krasnodar = "krasnodar"
  val Stage = "stage"
  val TestNet = "testnet"

  // All network commands
  def commands[F[_]: AquaIO: Async](implicit ec: ExecutionContext): Opts[F[ExitCode]] =
    Opts.subcommand(NetworkOpts.listModules[F]) orElse
      Opts.subcommand(NetworkOpts.listBlueprints[F]) orElse
      Opts.subcommand(NetworkOpts.listInterfacesByPeer[F]) orElse
      Opts.subcommand(NetworkOpts.listInterfaces[F]) orElse
      Opts.subcommand(NetworkOpts.getInterface[F]) orElse
      Opts.subcommand(NetworkOpts.getModuleInterface[F]) orElse
      Opts.subcommand(NetworkOpts.envCom[F])

  def peerOpt: Opts[String] =
    Opts
      .option[String]("peer", "PeerId", "p")

  def idOpt: Opts[String] =
    Opts
      .option[String]("id", "Service ID", "s")

  def envArg: Opts[String] =
    Opts
      .argument[String]()
      .withDefault(Krasnodar)

  def listModules[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "listModules",
      header = "Print all modules"
    ) {
      GeneralRunOptions.commonOpt.map { common =>
        PlatformOpts.getPackagePath(NetworkAqua).flatMap { networkAquaPath =>
          RunOpts.execRun(
            common,
            ListModulesFuncName,
            networkAquaPath
          )
        }
      }
    }

  def listBlueprints[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "listBlueprints",
      header = "Print all blueprints"
    ) {
      GeneralRunOptions.commonOpt.map { common =>
        PlatformOpts.getPackagePath(NetworkAqua).flatMap { networkAquaPath =>
          RunOpts.execRun(
            common,
            ListBlueprintsFuncName,
            networkAquaPath
          )
        }

      }
    }

  def listInterfacesByPeer[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "listInterfaces",
      header = "Print all services on a node owned by peer"
    ) {
      (GeneralRunOptions.commonOpt, AppOpts.wrapWithOption(peerOpt)).mapN { (common, peer) =>
        PlatformOpts.getPackagePath(NetworkAqua).flatMap { networkAquaPath =>
          RunOpts.execRun(
            common,
            ListInterfacesByPeerFuncName,
            networkAquaPath,
            Nil,
            peer.map(LiteralRaw.quote).getOrElse(ValueRaw.InitPeerId) :: Nil
          )
        }

      }
    }

  def listInterfaces[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "listAllInterfaces",
      header = "Print all services on a node"
    ) {
      (GeneralRunOptions.commonOpt).map { common =>
        PlatformOpts.getPackagePath(NetworkAqua).flatMap { networkAquaPath =>
          RunOpts.execRun(
            common,
            ListInterfacesFuncName,
            networkAquaPath,
            Nil,
            Nil
          )
        }

      }
    }

  def getInterface[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "getInterface",
      header = "Print a service interface"
    ) {
      (GeneralRunOptions.commonOpt, idOpt).mapN { (common, serviceId) =>
        PlatformOpts.getPackagePath(NetworkAqua).flatMap { networkAquaPath =>
          RunOpts.execRun(
            common,
            GetInterfaceFuncName,
            networkAquaPath,
            Nil,
            LiteralRaw.quote(serviceId) :: Nil
          )
        }

      }
    }

  def getModuleInterface[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "getModuleInterface",
      header = "Print a module interface"
    ) {
      (GeneralRunOptions.commonOpt, idOpt).mapN { (common, serviceId) =>
        PlatformOpts.getPackagePath(NetworkAqua).flatMap { networkAquaPath =>
          RunOpts.execRun(
            common,
            GetModuleInterfaceFuncName,
            networkAquaPath,
            Nil,
            LiteralRaw.quote(serviceId) :: Nil
          )
        }
      }
    }

  def envCom[F[_]: Applicative](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "env",
      header = "Show nodes in currently selected environment"
    ) {
      envArg.map { env =>
        val envList = env match {
          case Krasnodar =>
            Some(FluenceEnvironment.krasnodar)
          case TestNet =>
            Some(FluenceEnvironment.testnet)
          case Stage =>
            Some(FluenceEnvironment.stage)
          case e =>
            None
        }

        envList match {
          case Some(envs) =>
            println(envs.toList.map(n => n.selectDynamic("multiaddr")).mkString("\n"))
            ExitCode.Success.pure[F]
          case None =>
            println(
              s"There is no environment '$env' in our list. Use this: 'krasnodar', 'testnet', 'stage'"
            )
            ExitCode.Success.pure[F]
        }
      }
    }

}
