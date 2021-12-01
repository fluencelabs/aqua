package aqua

import aqua.Utils.logger
import aqua.js.{Meta, Module}
import fs2.io.file.Path
import scribe.Logging

import scala.util.Try

// Java-specific functions
object PlatformUtils extends Logging {
  // you need to add import with builtin.aqua manually when running in JVM environment 
  def getBuiltinNodeModulePaths: Option[Path] = None
}
