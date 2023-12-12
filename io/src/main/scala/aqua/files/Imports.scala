package aqua.files

import fs2.io.file.Path
import scala.util.Try

/**
 * Imports resolution configuration.
 */
final case class Imports(
  settings: Map[Path, Imports.PathSettings]
) {

  /**
   * Get all possible resolutions for a given import.
   *
   * @param from path of the file that imports
   * @param imported import string
   * @return list of possible resolutions
   */
  def resolutions(from: Path, imported: String): List[Path] =
    relative(from, imported).toList ::: gather(from, imported)

  // Return relative resolution if possible
  private def relative(from: Path, imported: String): Option[Path] =
    for {
      fromParent <- from.parent
      importedPath <- Try(Path(imported)).toOption
    } yield fromParent.resolve(importedPath)

  // Gather all possible resolutions from imports config
  private def gather(from: Path, imported: String): List[Path] = {
    val fromNorm = from.normalize.absolute

    // First - find the longest matching prefix for path
    settings.filter { case (prefix, _) =>
      fromNorm.startsWith(prefix.normalize.absolute)
    }.maxByOption { case (prefix, _) =>
      prefix.toString.length
    }.flatMap { case (_, s) =>
      // Then - find the longest matching prefix for import
      s.imports.filter { case (prefix, _) =>
        imported.startsWith(prefix)
      }.maxByOption { case (prefix, _) =>
        prefix.length
      }
    }.map { case (prefix, paths) =>
      // Drop the prefix from import and append to the path
      val dropped = imported.drop(prefix.length)
      paths.map(_ / dropped)
    }.toList.flatten
  }
}

object Imports {

  final case class PathSettings(
    imports: Map[String, List[Path]]
  )
}
