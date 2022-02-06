package aqua.js

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSImport}

object Config {

  @js.native
  @JSImport("./dist/config.js", "fillWithEmptyArrays")
  def fillWithEmptyArrays(
    config: js.Dynamic
  ): js.Dynamic = js.native
}
