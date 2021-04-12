package aqua.io

import aqua.AquaError
import aqua.parser.lift.FileSpan
import cats.data.NonEmptyChain

import java.nio.file.Path

sealed trait AquaFileError

case class FileNotFound(focus: FileSpan.Focus, name: Path, imports: Seq[Path]) extends AquaFileError
case class EmptyFileError(path: Path) extends AquaFileError
case class FileSystemError(err: Throwable) extends Exception(err) with AquaFileError

case class AquaScriptErrors(name: String, script: String, errors: NonEmptyChain[AquaError])
    extends AquaFileError
