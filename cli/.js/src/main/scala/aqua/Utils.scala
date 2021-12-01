package aqua

import aqua.js.{AvmLogLevel, FluenceJSLogLevel, Meta, Module}
import fs2.io.file.Path
import scribe.{Level, Logging}

import scala.util.Try

object Utils extends Logging {

  // get path to node modules if there is `aqua-lib` module with `builtin.aqua` in it
  def getBuiltinNodeModulePaths: Option[Path] = {
    val meta = Meta.metaUrl
    val req = Module.createRequire(meta)
    Try {
      // this can throw an error
      val pathStr = req.resolve("@fluencelabs/aqua-lib/builtin.aqua").toString
      // hack
      Path(pathStr).parent.flatMap(_.parent).flatMap(_.parent)
    }.getOrElse {
      // we don't care about path if there is no builtins, but must write an error
      logger.error("Unexpected. Cannot find 'aqua-lib' dependency with `builtin.aqua` in it")
      None
    }

  }

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
