package aqua.ipfs.js

import aqua.js.FluencePeer

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSImport}

object IpfsApi {

  @js.native
  @JSImport("./dist/src/ipfs.js", "uploadFile")
  def uploadFile(path: String, provider: FluencePeer): js.Promise[String] = js.native
}
