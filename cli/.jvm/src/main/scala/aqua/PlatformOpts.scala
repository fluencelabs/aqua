package aqua

import cats.data.ValidatedNec
import cats.effect.ExitCode
import cats.effect.kernel.Async
import cats.effect.std.Console
import com.monovore.decline.Opts
import fs2.io.file.{Files, Path}

import scala.concurrent.ExecutionContext

// Scala-specific options and subcommands
object PlatformOpts {

  def opts[F[_]: Files: AquaIO: Async: Console](implicit
    ec: ExecutionContext
  ): Opts[F[ValidatedNec[String, Unit]]] = Opts.never
  def getGlobalNodeModulePath: List[Path] = Nil
  def getPackagePath: Option[Path] = None
}
