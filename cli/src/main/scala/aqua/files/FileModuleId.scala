package aqua.files

import fs2.io.file.Path
import cats.Order

case class FileModuleId private (file: Path) {
  override def toString: String = s"${file}"
}

object FileModuleId {

  implicit object FileModuleIdOrder extends Order[FileModuleId] {

    override def compare(x: FileModuleId, y: FileModuleId): Int =
      x.file.toString.compareTo(y.file.toString)
  }

  def apply(file: Path): FileModuleId =
    new FileModuleId(file.absolute.normalize)
}
