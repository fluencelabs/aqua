package aqua.files

import java.nio.file.Path

case class FileModuleId private (file: Path)

object FileModuleId {

  def apply(file: Path): FileModuleId =
    new FileModuleId(file.toAbsolutePath.normalize())
}
