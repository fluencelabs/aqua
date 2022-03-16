package aqua.network

import aqua.builder.IPFSUploader
import aqua.dist.DistOpts.*
import aqua.files.AquaFilesIO
import aqua.ipfs.IpfsOpts.{pathOpt, UploadFuncName}
import aqua.js.FluenceEnvironment
import aqua.model.{LiteralModel, ValueModel}
import aqua.raw.value.{LiteralRaw, ValueRaw}
import aqua.run.{GeneralRunOptions, RunCommand, RunConfig, RunOpts}
import aqua.*
import cats.Applicative
import cats.data.{NonEmptyList, Validated}
import Validated.{invalidNel, validNel}
import cats.effect.ExitCode
import cats.effect.kernel.Async
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import com.monovore.decline.{Command, Opts}
import fs2.io.file.Path

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
      ),
      listPeers :: Nil
    ).command

  def ownerOpt: Opts[String] =
    Opts
      .option[String]("owner", "PeerId", "o")

  def allFlag: Opts[Boolean] =
    Opts
      .flag("all", "Get all services on a node")
      .map(_ => true)
      .withDefault(false)

  def idOpt: Opts[String] =
    Opts
      .option[String]("id", "Service ID", "s")

  def envArg: Opts[js.Array[js.Dynamic]] =
    Opts
      .argument[String]("krasnodar | stage | testnet")
      .withDefault(Krasnodar)
      .mapValidated {
        case Krasnodar =>
          validNel(FluenceEnvironment.krasnodar)
        case TestNet =>
          validNel(FluenceEnvironment.testnet)
        case Stage =>
          validNel(FluenceEnvironment.stage)
        case e =>
          invalidNel(
            s"There is no environment '$e' in our list. Use this: 'krasnodar', 'testnet', 'stage'"
          )
      }

  def listModules[F[_]: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.simple(
      ListModulesFuncName,
      "List all modules on a peer",
      PackagePath(NetworkAqua),
      ListModulesFuncName
    )

  def listBlueprints[F[_]: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.simple(
      ListBlueprintsFuncName,
      "List all blueprints on a peer",
      PackagePath(NetworkAqua),
      ListBlueprintsFuncName
    )

  def listInterfaces[F[_]: Async]: SubCommandBuilder[F] =
    SubCommandBuilder.valid(
      "list_interfaces",
      "List all service interfaces on a peer by a given owner",
      (GeneralRunOptions.commonOpt, AppOpts.wrapWithOption(ownerOpt), allFlag).mapN {
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
      "Show interface of a service",
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

  def listPeers[F[_]: Applicative]: Command[F[ExitCode]] =
    Command(
      name = "list_peers",
      header = "List addresses of bootstrap peers"
    ) {
      envArg.map { env =>
        println(env.toList.map(n => n.selectDynamic("multiaddr")).mkString("\n"))
        ExitCode.Success.pure[F]
      }
    }

}
