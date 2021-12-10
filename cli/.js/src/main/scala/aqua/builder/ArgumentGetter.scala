package aqua.builder

import aqua.js.{CallJsFunction, CallServiceHandler, FluencePeer}
import aqua.model.func.Call
import aqua.model.func.raw.CallServiceTag
import aqua.model.{LiteralModel, VarModel}

import scala.concurrent.Promise

// Service that can return argument to use it from a code
case class ArgumentGetter(serviceId: String, value: VarModel, arg: scalajs.js.Dynamic)
    extends ServiceFunction {

  def registerService(peer: FluencePeer): CallServiceHandler = {
    CallJsFunction.registerService(
      peer,
      serviceId,
      value.name,
      _ => arg
    )
  }

  def callTag(): CallServiceTag = {
    CallServiceTag(
      LiteralModel.quote(serviceId),
      value.name,
      Call(List.empty, List(Call.Export(value.name, value.`type`)))
    )
  }

}

object ArgumentGetter {

  val ServiceId = "getDataSrv"

  def apply(value: VarModel, arg: scalajs.js.Dynamic): ArgumentGetter = {
    ArgumentGetter(ServiceId, value, arg)
  }
}
