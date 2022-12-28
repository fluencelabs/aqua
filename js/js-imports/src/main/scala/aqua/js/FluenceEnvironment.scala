package aqua.js

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSImport}

@js.native
@JSImport("@fluencelabs/fluence-network-environment/dist/index.js", "Node")
class FluenceNode extends js.Object {
  val multiaddr: String = js.native
  def peerId: String = js.native
}

object FluenceEnvironment {

  @js.native
  @JSImport("@fluencelabs/fluence-network-environment/dist/index.js", "stage")
  val stage: js.Array[FluenceNode] = js.native

  @js.native
  @JSImport("@fluencelabs/fluence-network-environment/dist/index.js", "krasnodar")
  val krasnodar: js.Array[FluenceNode] = js.native

  @js.native
  @JSImport("@fluencelabs/fluence-network-environment/dist/index.js", "testNet")
  val testnet: js.Array[FluenceNode] = js.native
}
