package aqua.files

import fs2.io.file.Path

final case class Imports(
  settings: Map[Path, Imports.PathSettings]
)

object Imports {

  final case class PathSettings(
    imports: Map[String, List[Path]]
  )
}
