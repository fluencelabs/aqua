package aqua

import cats.effect.ExitCode
import cats.effect.kernel.Async
import com.monovore.decline.Opts
import fs2.io.file.Files
import scala.concurrent.ExecutionContext

import aqua.run.RunOpts
import aqua.keypair.KeyPairOpts

// JS-specific options and subcommands
object PlatformOpts {
    def opts[F[_]: Files: AquaIO: Async](implicit ec: ExecutionContext): Opts[F[ExitCode]] = 
        Opts.subcommand(RunOpts.runCommand[F]) orElse 
        Opts.subcommand(KeyPairOpts.createKeypair[F])
}

