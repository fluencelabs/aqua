package aqua

import aqua.ArgGetterService.GETTER_SERVICE_ID
import aqua.js.{CallJsFunction, CallServiceHandler, FluencePeer}
import aqua.model.func.Call
import aqua.model.{LiteralModel, VarModel}
import aqua.model.func.raw.CallServiceTag

import scala.concurrent.Promise

case class ArgGetterService(fnName: String, arg: scalajs.js.Object) {

  def registerService(peer: FluencePeer): CallServiceHandler = {
    CallJsFunction.registerService(
      peer,
      GETTER_SERVICE_ID,
      fnName,
      _ => {
        arg
      }
    )
  }

  def getCallServiceTag(): CallServiceTag = {
    CallServiceTag(
      LiteralModel.quote(GETTER_SERVICE_ID),
      fnName,
      Call(List.empty, Nil)
    )
  }

}

object ArgGetterService {

  val GETTER_SERVICE_ID = "argumentGetter"

  def create(argName: String, arg: scalajs.js.Object): ArgGetterService = {
    val fnName = s"get${argName}Fn"
    ArgGetterService(fnName, arg)
  }
}
