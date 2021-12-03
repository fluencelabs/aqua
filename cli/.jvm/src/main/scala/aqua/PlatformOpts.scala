package aqua

import cats.effect.ExitCode
import com.monovore.decline.Opts
import fs2.io.file.Path

// Scala-specific options and subcommands
object PlatformOpts {
  def opts[F[_]]: Opts[F[ExitCode]] = Opts.never
  def getGlobalNodeModulePath: Option[Path] = None
}
