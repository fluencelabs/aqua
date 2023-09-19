package aqua.errors

import sourcecode.{Enclosing, FileName, Line}

import scala.util.control.NoStackTrace

object Errors {

  /**
   * Internal error that should never happen.
   * Use in case of broken invariants.
   */
  def internalError(
    msg: String
  )(using file: FileName, line: Line, enclosing: Enclosing): Nothing = {
    throw new RuntimeException(
      s"Internal aqua compiler error:\n$msg" +
        s"\nat ${file.value}:${line.value} in ${enclosing.value}.\n" +
        s"Please report this issue to https://github.com/fluencelabs/aqua."
    ) with NoStackTrace // Stack trace is rather useless here
  }
}
