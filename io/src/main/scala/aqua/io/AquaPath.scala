package aqua.io

import aqua.PlatformPackagePath

import cats.effect.kernel.Async
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monad.*
import fs2.io.file.Path

sealed trait AquaPath {
  def getPath[F[_]: Async](): F[Path]
}

// Path for package relative files
case class PackagePath(path: String) extends AquaPath {
  def getPath[F[_]: Async](): F[Path] = PlatformPackagePath.getPackagePath(path)
}

// Path for absolute or call path relative files
case class RelativePath(path: Path) extends AquaPath {
  def getPath[F[_]: Async](): F[Path] = path.pure[F]
}

object PackagePath {
  // path to a builtin file in aqua package
  val builtin: PackagePath = PackagePath("../aqua-lib/builtin.aqua")
}
