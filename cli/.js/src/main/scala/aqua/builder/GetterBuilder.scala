package aqua.builder

import aqua.js.{CallJsFunction, CallServiceHandler, FluencePeer}
import aqua.model.func.Call
import aqua.model.func.raw.CallServiceTag
import aqua.model.{LiteralModel, VarModel}

import scala.concurrent.Promise

// Service that can return argument to use it from a code
case class GetterBuilder(serviceId: String, value: VarModel, arg: scalajs.js.Dynamic) {

  def registerService(peer: FluencePeer): CallServiceHandler = {
    CallJsFunction.registerService(
      peer,
      serviceId,
      value.name,
      _ => {
        arg
      }
    )
  }

  def getCallServiceTag(): CallServiceTag = {
    CallServiceTag(
      LiteralModel.quote(serviceId),
      value.name,
      Call(List.empty, List(Call.Export(value.name, value.`type`)))
    )
  }

}

object GetterBuilder {

  val GETTER_SERVICE_ID = "getDataSrv"

  def create(value: VarModel, arg: scalajs.js.Dynamic): GetterBuilder = {
    GetterBuilder(GETTER_SERVICE_ID, value, arg)
  }
}
