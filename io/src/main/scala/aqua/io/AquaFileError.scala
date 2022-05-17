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

case class FileNotFound(name: Path, imports: Seq[Path]) extends AquaFileError {

  override def showForConsole: String =
    if (imports.nonEmpty)
      s"File '$name' not found, looking in ${imports.mkString(", ")}"
    else
      s"File '$name' not found"
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
