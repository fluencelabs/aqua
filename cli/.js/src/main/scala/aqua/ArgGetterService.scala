package aqua

import aqua.ArgGetterService.GETTER_SERVICE_ID
import aqua.js.{CallJsFunction, CallServiceHandler, FluencePeer}
import aqua.model.func.Call
import aqua.model.{LiteralModel, VarModel}
import aqua.model.func.raw.CallServiceTag

import scala.concurrent.Promise

// Service that can return argument to use it from a code
case class ArgGetterService(value: VarModel, arg: scalajs.js.Dynamic) {

  def registerService(peer: FluencePeer): CallServiceHandler = {
    CallJsFunction.registerService(
      peer,
      GETTER_SERVICE_ID,
      value.name,
      _ => {
        arg
      }
    )
  }

  def getCallServiceTag(): CallServiceTag = {
    CallServiceTag(
      LiteralModel.quote(GETTER_SERVICE_ID),
      value.name,
      Call(List.empty, List(Call.Export(value.name, value.`type`)))
    )
  }

}

object ArgGetterService {

  val GETTER_SERVICE_ID = "getDataSrv"

  def create(value: VarModel, arg: scalajs.js.Dynamic): ArgGetterService = {
    ArgGetterService(value, arg)
  }
}
