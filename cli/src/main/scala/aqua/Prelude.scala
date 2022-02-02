package aqua

import cats.Monad
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import fs2.io.file.{Files, Path}
import scribe.Logging

import scala.util.Try

/**
 * @param importPaths list of paths where imports will be searched
 */
case class Prelude(importPaths: List[Path])

// JS-specific functions
object Prelude extends Logging {

  lazy val runImports: List[Path] = Path("aqua/run-builtins") :: Nil

  def init[F[_]: Files: Monad](withRunImports: Boolean = false): F[Prelude] = {
    // check if node_modules directory exists and add it in imports list
    val nodeModules = Path("node_modules")
    val nodeImportF: F[Option[Path]] = Files[F].exists(nodeModules).flatMap {
      case true =>
        Files[F].isDirectory(nodeModules).map(isDir => if (isDir) Some(nodeModules) else None)
      case false => None.pure[F]
    }

    nodeImportF.map { nodeImport =>
      val imports = nodeImport.toList ++ PlatformOpts.getGlobalNodeModulePath.toList ++ runImports

      new Prelude(imports)
    }
  }

}
