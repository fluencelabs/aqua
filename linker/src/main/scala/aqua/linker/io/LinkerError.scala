package aqua.linker.io

import java.nio.file.Path

sealed trait LinkerError

case class FileNotFound(path: Path, imports: Seq[Path]) extends LinkerError
case class FileSystemError(err: Throwable) extends Exception(err) with LinkerError
