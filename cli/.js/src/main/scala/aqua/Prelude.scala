package aqua

import aqua.LogLevelTransformer
import aqua.js.{Meta, Module}
import fs2.io.file.Path
import scribe.Logging

import scala.util.Try

/**
 * @param pathList list of paths where imports will be searched
 */
case class Prelude(pathList: List[Path])

// JS-specific functions
object Prelude extends Logging {

  def apply(): Prelude = {
    new Prelude(getGlobalNodeModulePaths.toList)
  }

  // get path to node modules if there is `aqua-lib` module with `builtin.aqua` in it
  def getGlobalNodeModulePaths: Option[Path] = {
    val meta = Meta.metaUrl
    val req = Module.createRequire(meta)
    Try {
      // this can throw an error
      val pathStr = req.resolve("@fluencelabs/aqua-lib/builtin.aqua").toString
      // hack
      Path(pathStr).parent.map(_.resolve("../.."))
    }.getOrElse {
      // we don't care about path if there is no builtins, but must write an error
      logger.error("Unexpected. Cannot find 'aqua-lib' dependency with `builtin.aqua` in it")
      None
    }

  }
}
