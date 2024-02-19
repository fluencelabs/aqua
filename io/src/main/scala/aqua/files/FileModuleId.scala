package aqua.files

import aqua.semantics.FileId
import fs2.io.file.Path
import cats.{Order, Show}

case class FileModuleId private (file: Path) {
  override def toString: String = s"$file"
}

object FileModuleId {

  given FileId[FileModuleId] with {
    override def compare(x: FileModuleId, y: FileModuleId): Int =
      x.file.toString.compareTo(y.file.toString)

    override def show(t: FileModuleId): String = t.toString
  }

  def apply(file: Path): FileModuleId =
    new FileModuleId(file.absolute.normalize)
}
