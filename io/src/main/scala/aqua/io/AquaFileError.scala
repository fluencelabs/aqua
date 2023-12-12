package aqua.io

import cats.data.NonEmptyChain
import fs2.io.file.Path

sealed trait AquaFileError {
  def showForConsole: String

  override def toString: String = showForConsole
}

case class ListAquaErrors(errors: NonEmptyChain[AquaFileError]) extends AquaFileError {

  override def showForConsole: String =
    s"Cannot read '*.aqua' files:\n" + errors.map(_.showForConsole)
}

case class FileNotFound(path: Path) extends AquaFileError {
  override def showForConsole: String = s"File not found: $path"
}

case class ImportUnresolved(name: String, resolutions: Seq[Path]) extends AquaFileError {

  override def showForConsole: String =
    if (resolutions.nonEmpty)
      s"Import '$name' could not be resolved, tried: ${resolutions.mkString(", ")}"
    else
      s"Import '$name' could not be resolved"
}

case class FilesUnresolved(files: Seq[Path]) extends AquaFileError {

  def toImportUnresolved(name: String): ImportUnresolved =
    ImportUnresolved(name, files)

  override def showForConsole: String =
    s"Cannot resolve any of files: ${files.mkString(", ")}"
}

case class EmptyFileError(path: Path) extends AquaFileError {
  override def showForConsole: String = s"Path is empty: $path"
}

case class FileSystemError(err: Throwable) extends Exception(err) with AquaFileError {
  override def showForConsole: String = s"File system error: $err"
}

case class FileWriteError(file: Path, err: Throwable) extends Exception(err) with AquaFileError {
  override def showForConsole: String = s"Cannot write a file $file: $err"
}

case class Unresolvable(msg: String) extends AquaFileError {
  override def showForConsole: String = s"Unresolvable: $msg"
}
