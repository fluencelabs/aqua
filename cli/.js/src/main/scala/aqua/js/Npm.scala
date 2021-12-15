package aqua.js

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSImport}

object Meta {

  // get `import`.meta.url info from javascript
  // it is needed for `createRequire` function
  @js.native
  @JSImport("./utils.js", "metaUrl")
  val metaUrl: String = js.native
}

@js.native
@JSImport("module", JSImport.Namespace)
object Module extends js.Object {

  // make it possible to use `require` in ES module type
  def createRequire(str: String): Require = js.native
}

trait Require extends js.Object {
  def resolve(str: String): Any
}
