package aqua.io

import aqua.AquaError
import aqua.parser.lift.FileSpan
import cats.data.NonEmptyChain

import java.nio.file.Path

sealed trait AquaFileError {
  def showForConsole: String

  override def toString: String = showForConsole
}

case class FileNotFound(focus: FileSpan.Focus, name: Path, imports: Seq[Path])
    extends AquaFileError {

  override def showForConsole: String = focus.toConsoleStr(
    s"File not found at $name, looking in ${imports.mkString(", ")}",
    Console.YELLOW
  )
}

case class EmptyFileError(path: Path) extends AquaFileError {
  override def showForConsole: String = s"Path is empty: $path"
}

case class FileSystemError(err: Throwable) extends Exception(err) with AquaFileError {
  override def showForConsole: String = s"File system error: ${err.getMessage}"
}

case class Unresolvable(msg: String) extends AquaFileError {
  override def showForConsole: String = s"Unresolvable: $msg"
}

// TODO there should be no AquaErrors, as they does not fit
case class AquaScriptErrors(errors: NonEmptyChain[AquaError]) extends AquaFileError {

  override def showForConsole: String =
    errors.map(_.showForConsole).toChain.toList.mkString("\n")
}
