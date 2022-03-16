package aqua.network

import aqua.{
  AppOpts,
  AquaIO,
  CliFunc,
  CommandBuilder,
  FluenceOpts,
  LogFormatter,
  PackagePath,
  PlatformOpts,
  RunInfo,
  SubCommandBuilder
}
import aqua.builder.IPFSUploader
import aqua.dist.DistOpts.*
import aqua.files.AquaFilesIO
import aqua.ipfs.IpfsOpts.{pathOpt, UploadFuncName}
import aqua.model.{LiteralModel, ValueModel}
import aqua.raw.value.{LiteralRaw, ValueRaw}
import aqua.run.{GeneralRunOptions, RunCommand, RunConfig, RunOpts}
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
import cats.data.NonEmptyList

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
  def commands[F[_]: AquaIO: Async]: Command[F[ExitCode]] =
    CommandBuilder(
      "net",
      "Create services and query network",
      NonEmptyList(
        deploy,
        remove :: createService :: addBlueprint :: listModules :: listBlueprints :: listInterfaces :: getInterface :: Nil
      )
    ).command

  def peerOpt: Opts[String] =
    Opts
      .option[String]("peer", "PeerId", "p")

  def allFlag: Opts[Boolean] =
    Opts
      .flag("all", "Get all services on a node")
      .map(_ => true)
      .withDefault(false)

  def idOpt: Opts[String] =
    Opts
      .option[String]("id", "Service ID", "s")

  def envArg: Opts[String] =
    Opts
      .argument[String]("krasnodar | stage | testnet")
      .withDefault(Krasnodar)

  def listModules[F[_]: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.simple(
      ListModulesFuncName,
      "Print all modules",
      PackagePath(NetworkAqua),
      ListModulesFuncName
    )

  def listBlueprints[F[_]: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.simple(
      ListBlueprintsFuncName,
      "Print all blueprints",
      PackagePath(NetworkAqua),
      ListBlueprintsFuncName
    )

  def listInterfaces[F[_]: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.valid(
      "list_interfaces",
      "Print all services on a node owned by peer",
      (GeneralRunOptions.commonOpt, AppOpts.wrapWithOption(peerOpt), allFlag).mapN {
        (common, peer, printAll) =>
          if (printAll)
            RunInfo(
              common,
              CliFunc(
                ListInterfacesFuncName,
                Nil
              ),
              PackagePath(NetworkAqua)
            )
          else
            RunInfo(
              common,
              CliFunc(
                ListInterfacesByPeerFuncName,
                peer.map(LiteralRaw.quote).getOrElse(ValueRaw.InitPeerId) :: Nil
              ),
              PackagePath(NetworkAqua)
            )
      }
    )

  def getInterface[F[_]: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.valid(
      GetInterfaceFuncName,
      "Print a service interface",
      (GeneralRunOptions.commonOpt, idOpt).mapN { (common, serviceId) =>
        RunInfo(
          common,
          CliFunc(GetInterfaceFuncName, LiteralRaw.quote(serviceId) :: Nil),
          PackagePath(NetworkAqua)
        )
      }
    )

  def getModuleInterface[F[_]: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.valid(
      GetModuleInterfaceFuncName,
      "Print a module interface",
      (GeneralRunOptions.commonOpt, idOpt).mapN { (common, serviceId) =>
        RunInfo(
          common,
          CliFunc(GetModuleInterfaceFuncName, LiteralRaw.quote(serviceId) :: Nil),
          PackagePath(NetworkAqua)
        )
      }
    )

  def envCom[F[_]: Applicative]: Command[F[ExitCode]] =
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
