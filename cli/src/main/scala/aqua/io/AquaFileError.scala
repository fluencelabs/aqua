package aqua.io

import cats.data.NonEmptyChain

import java.nio.file.Path

sealed trait AquaFileError {
  def showForConsole: String

  override def toString: String = showForConsole
}

case class FileNotFound(name: Path, imports: Seq[Path]) extends AquaFileError {

  override def showForConsole: String =
    s"File not found at $name, looking in ${imports.mkString(", ")}"
}

case class EmptyFileError(path: Path) extends AquaFileError {
  override def showForConsole: String = s"Path is empty: $path"
}

case class FileSystemError(err: Throwable) extends Exception(err) with AquaFileError {
  override def showForConsole: String = s"File system error: ${err.getMessage}"
}

case class FileWriteError(file: Path, err: Throwable) extends Exception(err) with AquaFileError {
  override def showForConsole: String = s"Cannot write a file $file: ${err.getMessage}"
}

case class Unresolvable(msg: String) extends AquaFileError {
  override def showForConsole: String = s"Unresolvable: $msg"
}

// TODO there should be no AquaErrors, as they does not fit
case class AquaScriptErrors(errors: NonEmptyChain[AquaFileSpanError]) extends AquaFileError {

  override def showForConsole: String =
    errors.map(_.showForConsole).toChain.toList.mkString("\n")
}
