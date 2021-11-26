package aqua

import cats.effect.ExitCode
import com.monovore.decline.Opts

// Scala-specific options and subcommands
object PlatformOpts {
    def opts[F[_]]: Opts[F[ExitCode]] = Opts.never
}
