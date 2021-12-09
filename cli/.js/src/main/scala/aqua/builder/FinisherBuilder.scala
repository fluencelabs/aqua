package aqua.builder

import aqua.io.OutputPrinter
import aqua.js.{CallJsFunction, FluencePeer}
import aqua.model.func.Call
import aqua.model.func.raw.CallServiceTag
import aqua.model.{LiteralModel, VarModel}

import scala.concurrent.Promise
import scala.scalajs.js.JSON

// Will finish promise on service call
class FinisherBuilder private (
  serviceId: String,
  fnName: String,
  val promise: Promise[Unit]
) {

  def getCallServiceTag(): CallServiceTag = {
    CallServiceTag(
      LiteralModel.quote(serviceId),
      fnName,
      Call(Nil, Nil)
    )
  }

  def registerService(peer: FluencePeer) = {
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

object FinisherBuilder {

  def apply(serviceId: String, fnName: String) =
    new FinisherBuilder(serviceId, fnName, Promise[Unit]())
}
