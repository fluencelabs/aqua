package aqua.builder

import aqua.js.{CallServiceHandler, FluencePeer}

trait ServiceFunction {
  def registerService(peer: FluencePeer): Unit
}

object ServiceFunction {
  val emptyObject = scalajs.js.Dynamic.literal
}
