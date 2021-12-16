package aqua.ipfs.js

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSImport}

@JSExportAll
case class GetExternalApiMultiaddrResult(
  error: String,
  multiaddr: String,
  success: Boolean
)

@JSExportAll
case class GetExternalSwarmMultiaddrResult(
  error: String,
  multiaddr: String,
  success: Boolean
)

object IpfsApi {

  @js.native
  @JSImport("compiled/", "get_external_api_multiaddr")
  def getExternalApiMultiaddr(node: String): js.Promise[GetExternalApiMultiaddrResult] = js.native

  @js.native
  @JSImport("compiled/", "get_external_swarm_multiaddr")
  def getExternalSwarmMultiaddr(node: String): js.Promise[GetExternalSwarmMultiaddrResult] =
    js.native
}
