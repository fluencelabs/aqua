package aqua.errors

import sourcecode.{Enclosing, FileName, Line}

object Errors {

  /**
   * Internal error that should never happen.
   * Use in case of broken invariants.
   */
  def internalError(
    msg: String
  )(using file: FileName, line: Line, enclosing: Enclosing): Nothing = {
    throw new RuntimeException(
      s"Internal aqua compiler error:" +
        s" $msg at ${file.value}:${line.value} in ${enclosing.value}.\n" +
        s"Please report this issue to https://github.com/fluencelabs/aqua."
    )
  }
}
