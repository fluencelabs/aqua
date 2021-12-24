package aqua.builder

import aqua.backend.{ArgDefinition, PrimitiveType, ServiceDef, ServiceFunctionDef}
import aqua.js.{CallJsFunction, CallServiceHandler, FluencePeer}
import aqua.model.func.Call
import aqua.model.func.raw.CallServiceTag
import aqua.model.{LiteralModel, VarModel}

import scalajs.js
import scala.concurrent.Promise

// Service that can return argument to use it from a code
case class ArgumentGetter(serviceId: String, value: VarModel, arg: scalajs.js.Dynamic)
    extends ServiceFunction {

  def registerService(peer: FluencePeer): Unit = {
    CallJsFunction.registerService(
      peer,
      serviceId,
      value.name,
      _ => js.Promise.resolve(arg),
      ServiceDef(
        None,
        ServiceFunctionDef(
          value.name,
          Nil,
          PrimitiveType
        ) :: Nil
      )
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
