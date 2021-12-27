package aqua.network

import aqua.{AppOpts, AquaIO, FluenceOpts, LogFormatter}
import aqua.builder.IPFSUploader
import aqua.files.AquaFilesIO
import aqua.ipfs.IpfsOpts.{pathOpt, IpfsAquaPath, UploadFuncName}
import aqua.model.{LiteralModel, ValueModel}
import aqua.run.{RunCommand, RunConfig, RunOpts}
import cats.effect.ExitCode
import cats.effect.kernel.Async
import com.monovore.decline.{Command, Opts}
import fs2.io.file.Path
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.apply.*

import scala.concurrent.ExecutionContext

object NetworkOpts {

  val NetworkAquaPath = "aqua/network-info.aqua"
  val ListModulesFuncName = "list_modules"
  val ListBlueprintsFuncName = "list_blueprints"
  val ListInterfacesByPeerFuncName = "list_interfaces_by_peer"
  val ListInterfacesFuncName = "list_services"
  val GetInterfaceFuncName = "get_interface"

  def peerOpt: Opts[String] =
    Opts
      .option[String]("peer", "PeerId", "p")

  def idOpt: Opts[String] =
    Opts
      .option[String]("id", "Service ID", "s")

  def listModules[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "listModules",
      header = "Get a list of modules"
    ) {
      FluenceOpts.commonOpt.map { common =>
        RunOpts.execRun(
          common,
          ListModulesFuncName,
          Path(NetworkAquaPath)
        )
      }
    }

  def listBlueprints[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "listBlueprints",
      header = "Get a list of blueprints"
    ) {
      FluenceOpts.commonOpt.map { common =>
        RunOpts.execRun(
          common,
          ListBlueprintsFuncName,
          Path(NetworkAquaPath)
        )
      }
    }

  def listInterfacesByPeer[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "listInterfaces",
      header = "Get a peer's list of interfaces"
    ) {
      (FluenceOpts.commonOpt, AppOpts.wrapWithOption(peerOpt)).mapN { (common, peer) =>
        RunOpts.execRun(
          common,
          ListInterfacesByPeerFuncName,
          Path(NetworkAquaPath),
          Nil,
          peer.map(LiteralModel.quote).getOrElse(LiteralModel.initPeerId) :: Nil
        )
      }
    }

  def listInterfaces[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "listAllInterfaces",
      header = "Get list of interfaces"
    ) {
      (FluenceOpts.commonOpt).map { common =>
        RunOpts.execRun(
          common,
          ListInterfacesFuncName,
          Path(NetworkAquaPath),
          Nil,
          Nil
        )
      }
    }

  def getInterface[F[_]: Async](implicit ec: ExecutionContext): Command[F[ExitCode]] =
    Command(
      name = "getInterface",
      header = "Get an interface by service id"
    ) {
      (FluenceOpts.commonOpt, idOpt).mapN { (common, serviceId) =>
        RunOpts.execRun(
          common,
          GetInterfaceFuncName,
          Path(NetworkAquaPath),
          Nil,
          LiteralModel.quote(serviceId) :: Nil
        )
      }
    }
}
