package aqua

import wvlet.log.{LogLevel => WLogLevel}

object LogLevel {

  val stringToLogLevel: Map[String, WLogLevel] = Map(
    ("debug" -> WLogLevel.DEBUG),
    ("trace" -> WLogLevel.TRACE),
    ("info" -> WLogLevel.INFO),
    ("off" -> WLogLevel.OFF),
    ("warn" -> WLogLevel.WARN),
    ("error" -> WLogLevel.ERROR),
    ("all" -> WLogLevel.ALL)
  )
}
