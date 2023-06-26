package aqua

import cats.effect.kernel.Async
import fs2.io.file.{Files, Path}
import aqua.js.Npm
import scribe.Logging
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monad.*

import scala.util.Try

object PlatformPackagePath extends Logging {

  // get path of nth parent strictly
  private def parentStrict(n: Int)(path: Path): Option[Path] =
    if (n <= 0) Some(path)
    else (0 until n).foldLeft(Option(path))((p, _) => p.flatMap(_.parent))

  // get path of nth parent relatively
  private def parentRelative(n: Int)(path: Path): Path =
    if (n <= 0) path
    else path.resolve((0 until n).map(_ => "../").mkString)

  // could throw an error
  private def builtinPath = Path(Npm.resolveModule("@fluencelabs/aqua-lib/builtin.aqua"))

  // it could be global installed aqua and local installed, different paths for this
  def getPackagePath[F[_]: Async](path: String): F[Path] = Try {
    val rootProjectPath = parentRelative(4)(builtinPath)
    // hack, check if it is a local dependency or global
    val filePath = rootProjectPath.resolve(path)
    Files[F].exists(filePath).map {
      // if not exists, it should be local dependency, check in node_modules
      case false => rootProjectPath.resolve("node_modules/@fluencelabs/aqua").resolve(path)
      case true => filePath
    }
  }.getOrElse {
    // we don't care about path if there is no builtins, but must write an error
    logger.error("Unexpected. Cannot find project path")
    Path(path).pure[F]
  }

  // get path to node modules if there is `aqua-lib` module with `builtin.aqua` in it
  def getGlobalNodeModulePath: List[Path] = Try {
    // hack
    val globalAquaPath = parentStrict(3)(builtinPath)

    // Also hack. If we found installed `aqua-lib`, it should be in `node_modules` global path.
    // In global `node_modules` could be installed aqua libs and we must use them,
    // if they were imported in aqua files
    val globalNodeModulesPath = globalAquaPath.flatMap(parentStrict(3))

    globalAquaPath.toList ++ globalNodeModulesPath.toList
  }.getOrElse {
    // we don't care about path if there is no builtins, but must write an error
    logger.error("Unexpected. Cannot find 'aqua-lib' dependency with `builtin.aqua` in it")
    Nil
  }

}
