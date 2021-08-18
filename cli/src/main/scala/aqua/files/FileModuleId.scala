package aqua.files

import fs2.io.file.Path

case class FileModuleId private (file: Path)

object FileModuleId {

  def apply(file: Path): FileModuleId =
    new FileModuleId(file.absolute.normalize)
}
