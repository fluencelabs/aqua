package aqua.builder

import aqua.backend.{ArgDefinition, PrimitiveType, ServiceDef, ServiceFunctionDef, TypeDefinition}
import aqua.js.{CallJsFunction, CallServiceHandler, FluencePeer, ServiceHandler}
import aqua.model.{LiteralModel, VarModel}
import aqua.raw.ops
import aqua.raw.ops.{Call, CallServiceTag}
import aqua.raw.value.{LiteralRaw, VarRaw}
import cats.data.NonEmptyList

import scalajs.js
import scala.concurrent.Promise

// Service that can return argument to use it from a code
abstract class ArgumentGetter(
  serviceId: String,
  val function: GetFunction
) extends Service(serviceId, NonEmptyList.one(function)) {

  def callTag(): CallServiceTag

}

case class GetFunction(value: VarRaw, arg: scalajs.js.Dynamic) extends AquaFunction {
  override def fnName: String = value.name

  def handler: ServiceHandler = _ => js.Promise.resolve(arg)
  def argDefinitions: List[ArgDefinition] = Nil
  def returnType: TypeDefinition = PrimitiveType
}

object ArgumentGetter {

  val ServiceId = "getDataSrv"

  private def getFunction(value: VarRaw, arg: scalajs.js.Dynamic) = GetFunction(value, arg)

  def apply(value: VarRaw, arg: scalajs.js.Dynamic): ArgumentGetter =
    new ArgumentGetter(ServiceId, getFunction(value, arg)) {

      override def callTag(): CallServiceTag =
        CallServiceTag(
          LiteralRaw.quote(ServiceId),
          value.name,
          Call(List.empty, List(Call.Export(value.name, value.baseType)))
        )
    }
}
