package aqua.js

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSImport}

@JSExportAll
case class PathOpts(local: Boolean = false)

@js.native
@JSImport("import", JSImport.Default)
object Imp extends js.Object {
  val meta: js.Dynamic = js.native
}

@js.native
@JSImport("module", JSImport.Default)
object Module extends js.Object {
  def createRequire(str: String): Require = js.native
}

trait Require {
  def resolve(str: String): String
}
