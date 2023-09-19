package aqua.errors

import scala.compiletime.sourcePosition

object Errors {

  def internalError(
    msg: String
  )(using source: sourcePosition): Nothing = {
    throw new RuntimeException(
      s"Internal aqua compiler error:" +
        s" $msg at ${source.fileName}:${source.lineNumber}" +
        s" in ${source.enclosingMethod}.\n" +
        s"Please report this issue to https://github.com/fluencelabs/aqua."
    )
  }
}
