package aqua.builder

import aqua.backend.{ServiceDef, ServiceFunctionDef, VoidType}
import aqua.io.OutputPrinter
import aqua.js.{CallJsFunction, FluencePeer}
import aqua.model.{LiteralModel, VarModel}
import aqua.raw.ops.{Call, CallServiceTag}
import aqua.raw.value.LiteralRaw

import scala.concurrent.Promise
import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.Dynamic

// Will finish promise on service call
case class Finisher private (
  serviceId: String,
  fnName: String,
  promise: Promise[Unit]
) extends ServiceFunction {

  def callTag(): CallServiceTag = {
    CallServiceTag(
      LiteralRaw.quote(serviceId),
      fnName,
      Call(Nil, Nil)
    )
  }

  def registerService(peer: FluencePeer) = {
    CallJsFunction.registerService(
      peer,
      serviceId,
      fnName,
      _ => {
        promise.success(())
        js.Promise.resolve(ServiceFunction.emptyObject)
      },
      ServiceDef(
        None,
        ServiceFunctionDef(
          fnName,
          Nil,
          VoidType
        ) :: Nil
      )
    )
  }
}

object Finisher {

  def apply(serviceId: String, fnName: String) =
    new Finisher(serviceId, fnName, Promise[Unit]())
}
