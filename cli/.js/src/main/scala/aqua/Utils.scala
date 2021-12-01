package aqua

import aqua.js.{AvmLogLevel, FluenceJSLogLevel, Meta, Module}
import fs2.io.file.Path
import scribe.{Level, Logging}

import scala.util.Try

object Utils extends Logging {

  def logLevelToAvm(logLevel: Level): AvmLogLevel = {
    logLevel match {
      case Level.Trace => "trace"
      case Level.Debug => "debug"
      case Level.Info => "info"
      case Level.Warn => "warn"
      case Level.Error => "error"
      case Level.Fatal => "off"
      case _ => "info"
    }
  }

  def logLevelToFluenceJS(logLevel: Level): FluenceJSLogLevel = {
    logLevel match {
      case Level.Trace => "trace"
      case Level.Debug => "debug"
      case Level.Info => "info"
      case Level.Warn => "warn"
      case Level.Error => "error"
      case Level.Fatal => "silent"
      case _ => "info"
    }
  }
}
