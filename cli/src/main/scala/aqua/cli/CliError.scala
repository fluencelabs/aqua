package aqua.cli

import aqua.AquaError
import cats.data.NonEmptyChain

import java.io.File

sealed trait CliError

object CliError {

  def parseError(name: String, error: String): CliError = {
    CliArgsError(name, error)
  }

  def errorInfo(name: String, script: String, errors: NonEmptyChain[AquaError]): CliError = {
    AquaScriptErrors(name, script, errors)
  }

  def ioError(msg: String, t: Throwable): CliError = {
    IOError(msg, t)
  }
}

case class IOError(msg: String, t: Throwable) extends Exception(msg, t) with CliError
case class EmptyFileError(file: File) extends CliError
case class CliArgsError(name: String, error: String) extends CliError

case class AquaScriptErrors(name: String, script: String, errors: NonEmptyChain[AquaError])
    extends CliError
