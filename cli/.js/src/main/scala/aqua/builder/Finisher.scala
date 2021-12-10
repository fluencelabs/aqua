package aqua.builder

import aqua.io.OutputPrinter
import aqua.js.{CallJsFunction, FluencePeer}
import aqua.model.func.Call
import aqua.model.func.raw.CallServiceTag
import aqua.model.{LiteralModel, VarModel}

import scala.concurrent.Promise
import scala.scalajs.js.JSON

// Will finish promise on service call
case class Finisher private (
  serviceId: String,
  fnName: String,
  promise: Promise[Unit]
) extends ServiceFunction {

  def callTag(): CallServiceTag = {
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

object Finisher {

  def apply(serviceId: String, fnName: String) =
    new Finisher(serviceId, fnName, Promise[Unit]())
}
