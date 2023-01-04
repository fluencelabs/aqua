package aqua.logging

import scribe.Level

object LogLevel {

  val stringToLogLevel: Map[String, Level] = Map(
    ("debug" -> Level.Debug),
    ("trace" -> Level.Trace),
    ("info" -> Level.Info),
    ("off" -> Level.Fatal),
    ("warn" -> Level.Warn),
    ("error" -> Level.Error),
    ("all" -> Level.Trace)
  )
}
