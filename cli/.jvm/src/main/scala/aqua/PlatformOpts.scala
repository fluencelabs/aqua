package aqua

import cats.effect.ExitCode
import com.monovore.decline.Opts
import fs2.io.file.Path
import cats.syntax.applicative._

// Scala-specific options and subcommands
object PlatformOpts {
  def opts[F[_]]: Opts[F[ExitCode]] = Opts.never
  def getGlobalNodeModulePath: Option[Path] = None
  def getPackagePath[F[_]: Files: Async](path: String): F[Path] = Path(path).pure[F]
}
