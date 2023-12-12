package aqua.files

import fs2.io.file.Path
import scala.util.Try

final case class Imports(
  settings: Map[Path, Imports.PathSettings]
) {

  def resolutions(from: Path, imported: String): List[Path] =
    relative(from, imported).toList ::: gather(from, imported)

  private def relative(from: Path, imported: String): Option[Path] =
    for {
      fromParent <- from.parent
      importedPath <- Try(Path(imported)).toOption
    } yield fromParent.resolve(importedPath)

  private def gather(from: Path, imported: String): List[Path] = {
    val fromNorm = from.normalize.absolute

    settings.filter { case (prefix, _) =>
      fromNorm.startsWith(prefix.normalize.absolute)
    }.maxByOption { case (prefix, _) =>
      prefix.toString.length
    }.flatMap { case (_, s) =>
      s.imports.filter { case (prefix, _) =>
        imported.startsWith(prefix)
      }.maxByOption { case (prefix, _) =>
        prefix.length
      }
    }.map { case (prefix, paths) =>
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
