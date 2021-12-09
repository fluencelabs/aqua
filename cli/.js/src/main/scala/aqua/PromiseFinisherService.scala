package aqua

import aqua.io.OutputPrinter
import aqua.js.{CallJsFunction, FluencePeer}
import aqua.model.{LiteralModel, VarModel}
import aqua.model.func.Call
import aqua.model.func.raw.CallServiceTag

import scala.concurrent.Promise
import scala.scalajs.js.JSON

class PromiseFinisherService(serviceId: String, fnName: String) {

  def getCallServiceTag(): CallServiceTag = {
    CallServiceTag(
      LiteralModel.quote(serviceId),
      fnName,
      Call(Nil, Nil)
    )
  }

  def registerService(peer: FluencePeer, promise: Promise[Unit]) = {
    CallJsFunction.registerUnitService(
      peer,
      serviceId,
      fnName,
      _ => {
        promise.success(())
      }
    )
  }
}
