package aqua.builder

import aqua.js.{CallServiceHandler, FluencePeer}
import scalajs.js.Dynamic

trait ServiceFunction {
  def registerService(peer: FluencePeer): Unit
}

object ServiceFunction {
  val emptyObject: Dynamic = Dynamic.literal()
}
