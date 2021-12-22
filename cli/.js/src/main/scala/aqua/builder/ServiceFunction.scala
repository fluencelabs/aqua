package aqua.builder

import aqua.js.{CallServiceHandler, FluencePeer}

trait ServiceFunction {
  def registerService(peer: FluencePeer): CallServiceHandler
}
