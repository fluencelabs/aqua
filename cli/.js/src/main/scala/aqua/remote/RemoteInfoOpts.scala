package aqua.remote

import aqua.builder.IPFSUploader
import DistOpts.*
import aqua.ipfs.IpfsOpts.{pathOpt, UploadFuncName}
import aqua.js.FluenceEnvironment
import aqua.model.{LiteralModel, ValueModel}
import aqua.raw.value.{LiteralRaw, ValueRaw}
import aqua.run.{GeneralOptions, RunCommand, RunConfig, RunOpts}
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

object RemoteInfoOpts {

  val NetworkAqua = "aqua/network-info.aqua"

  val ListModulesFuncName = "list_modules"
  val ListBlueprintsFuncName = "list_blueprints"
  val ListInterfacesByPeerFuncName = "list_interfaces_by_peer"
  val ListInterfacesFuncName = "list_services"
  val GetInterfaceFuncName = "get_interface"
  val GetModuleInterfaceFuncName = "get_module_interface"

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
      (GeneralOptions.opt, AppOpts.wrapWithOption(ownerOpt), allFlag).mapN {
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
      (GeneralOptions.opt, idOpt).mapN { (common, serviceId) =>
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
      (GeneralOptions.opt, idOpt).mapN { (common, serviceId) =>
        RunInfo(
          common,
          CliFunc(GetModuleInterfaceFuncName, LiteralRaw.quote(serviceId) :: Nil),
          PackagePath(NetworkAqua)
        )
      }
    )

}
