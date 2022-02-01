package aqua.builder

import aqua.backend.{ArgDefinition, PrimitiveType, ServiceDef, ServiceFunctionDef}
import aqua.js.{CallJsFunction, CallServiceHandler, FluencePeer}
import aqua.model.{LiteralModel, VarModel}
import aqua.raw.ops
import aqua.raw.ops.{Call, CallServiceTag}
import aqua.raw.value.{LiteralRaw, VarRaw}

import scalajs.js
import scala.concurrent.Promise

// Service that can return argument to use it from a code
case class ArgumentGetter(serviceId: String, value: VarRaw, arg: scalajs.js.Dynamic)
    extends ServiceFunction {

  def register(peer: FluencePeer): Unit = {
    CallJsFunction.registerService(
      peer,
      serviceId,
      (value.name, _ => js.Promise.resolve(arg)) :: Nil,
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

  def callTag(): CallServiceTag =
    CallServiceTag(
      LiteralRaw.quote(serviceId),
      value.name,
      Call(List.empty, List(Call.Export(value.name, value.baseType)))
    )

}

object ArgumentGetter {

  val ServiceId = "getDataSrv"

  def apply(value: VarRaw, arg: scalajs.js.Dynamic): ArgumentGetter =
    ArgumentGetter(ServiceId, value, arg)
}
