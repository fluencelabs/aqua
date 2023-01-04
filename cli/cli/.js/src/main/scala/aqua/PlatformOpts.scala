package aqua

import aqua.config.ConfigOpts
import aqua.ipfs.IpfsOpts
import aqua.keypair.KeyPairOpts
import aqua.remote.{DistOpts, RemoteOpts}
import aqua.run.RunOpts
import aqua.script.ScriptOpts
import cats.data.ValidatedNec
import cats.effect.ExitCode
import cats.effect.kernel.Async
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monad.*
import com.monovore.decline.Opts
import fs2.io.file.{Files, Path}
import scribe.Logging

import scala.concurrent.ExecutionContext
import scala.util.Try
import cats.effect.std.Console

// JS-specific options and subcommands
object PlatformOpts extends Logging {

  def opts[F[_]: Files: AquaIO: Async: Console]: Opts[F[ValidatedNec[String, Unit]]] =
    Opts.subcommand(RunOpts.runCommand[F]) orElse
      Opts.subcommand(KeyPairOpts.command[F]) orElse
      Opts.subcommand(IpfsOpts.ipfsOpt[F]) orElse
      Opts.subcommand(ScriptOpts.scriptOpt[F]) orElse
      Opts.subcommand(RemoteOpts.commands[F]) orElse
      Opts.subcommand(ConfigOpts.command[F])
}
