package aqua.ipfs.js

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSImport}

object IpfsApi {

  @js.native
  @JSImport("./dist/ipfs.js", "uploadFile")
  def uploadFile(
    path: js.Any,
    multiaddrResult: js.Any,
    infoLogger: js.Any,
    errorLogger: js.Any
  ): js.Promise[js.Dynamic] = js.native
}
