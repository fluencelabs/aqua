package aqua.remote

import aqua.{AquaIO, CommandBuilder}
import cats.data.{NonEmptyList, ValidatedNec}
import cats.effect.ExitCode
import cats.effect.kernel.Async
import com.monovore.decline.Command
import RemoteInfoOpts.*
import DistOpts.*

object RemoteOpts {

  // All remote commands
  def commands[F[_]: AquaIO: Async]: Command[F[ValidatedNec[String, Unit]]] =
    CommandBuilder(
      "remote",
      "Manage and query services on a remote peer",
      NonEmptyList(
        deploy,
        remove :: createService :: addBlueprint :: listModules :: listBlueprints :: listInterfaces :: getInterface :: Nil
      )
    ).command
}
