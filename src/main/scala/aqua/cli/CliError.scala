package aqua.cli

import aqua.AquaError
import cats.data.NonEmptyChain

sealed trait CliError

object CliError {

  def parseError(name: String, error: String): CliError = {
    ParseError(name, error)
  }

  def errorInfo(name: String, script: String, errors: NonEmptyChain[AquaError]): CliError = {
    ErrorInfo(name, script, errors)
  }

  def ioError(msg: String, t: Throwable): CliError = {
    IOError(msg, t)
  }
}

case class IOError(msg: String, t: Throwable) extends Exception(msg, t) with CliError
case class ParseError(name: String, error: String) extends CliError
case class ErrorInfo(name: String, script: String, errors: NonEmptyChain[AquaError]) extends CliError
