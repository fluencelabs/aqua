package aqua

import aqua.ipfs.IpfsOpts
import aqua.js.{Meta, Module}
import cats.effect.ExitCode
import cats.effect.kernel.Async
import com.monovore.decline.Opts
import fs2.io.file.{Files, Path}

import scala.concurrent.ExecutionContext
import aqua.run.RunOpts
import aqua.keypair.KeyPairOpts
import aqua.network.NetworkOpts
import scribe.Logging

import scala.util.Try

// JS-specific options and subcommands
object PlatformOpts extends Logging {

  def opts[F[_]: Files: AquaIO: Async](implicit ec: ExecutionContext): Opts[F[ExitCode]] =
    Opts.subcommand(RunOpts.runCommand[F]) orElse
      Opts.subcommand(KeyPairOpts.createKeypair[F]) orElse
      Opts.subcommand(IpfsOpts.ipfsOpt[F]) orElse
      NetworkOpts.commands[F]

  // get path to node modules if there is `aqua-lib` module with `builtin.aqua` in it
  def getGlobalNodeModulePath: Option[Path] = {
    val meta = Meta.metaUrl
    val req = Module.createRequire(meta)
    Try {
      // this can throw an error
      val pathStr = req.resolve("@fluencelabs/aqua-lib/builtin.aqua").toString
      // hack
      Path(pathStr).parent.map(_.resolve("../.."))
    }.getOrElse {
      // we don't care about path if there is no builtins, but must write an error
      logger.error("Unexpected. Cannot find 'aqua-lib' dependency with `builtin.aqua` in it")
      None
    }

  }
}
