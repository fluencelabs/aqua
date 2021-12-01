package aqua.js

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSImport}

object Meta {
  @js.native
  @JSImport("./utils.js", "metaUrl")
  val metaUrl: String = js.native
}

@js.native
@JSImport("module", JSImport.Namespace)
object Module extends js.Object {
  def createRequire(str: String): Require = js.native

  val paths: List[Any] = js.native
}

trait Require extends js.Object {
  def resolve(str: String): Any
}
