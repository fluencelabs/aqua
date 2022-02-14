package aqua.js

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object FluenceEnvironment {

  @js.native
  @JSImport("@fluencelabs/fluence-network-environment/dist/index.js", "stage")
  val stage: js.Array[js.Dynamic] = js.native

  @js.native
  @JSImport("@fluencelabs/fluence-network-environment/dist/index.js", "krasnodar")
  val krasnodar: js.Array[js.Dynamic] = js.native

  @js.native
  @JSImport("@fluencelabs/fluence-network-environment/dist/index.js", "testNet")
  val testnet: js.Array[js.Dynamic] = js.native
}
