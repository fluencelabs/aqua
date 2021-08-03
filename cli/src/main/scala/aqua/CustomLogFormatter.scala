package aqua

import wvlet.log.LogFormatter.{appendStackTrace, highlightLog}
import wvlet.log.{LogFormatter, LogRecord}

object CustomLogFormatter extends LogFormatter {

  override def formatLog(r: LogRecord): String = {
    val log =
      s"[${highlightLog(r.level, r.level.name)}] ${highlightLog(r.level, r.getMessage)}"
    appendStackTrace(log, r)
  }
}
