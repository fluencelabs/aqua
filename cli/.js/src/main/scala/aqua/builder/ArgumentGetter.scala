package aqua.builder

import aqua.backend.*
import aqua.js.{CallJsFunction, FluencePeer, ServiceHandler}
import aqua.model.{LiteralModel, VarModel}
import aqua.raw.ops
import aqua.raw.ops.{Call, CallArrowRawTag}
import aqua.raw.value.{LiteralRaw, VarRaw}
import cats.data.NonEmptyList

import scala.concurrent.Promise
import scala.scalajs.js

// Service that can return argument to use it from a code
// TODO: create one service with multiple argument getters instead of service per argument
abstract class ArgumentGetter(
  serviceId: String,
  val function: GetFunction
) extends Service(serviceId, NonEmptyList.one(function)) {

  def callTag(): CallArrowRawTag

}

case class GetFunction(value: VarRaw, arg: scalajs.js.Dynamic) extends AquaFunction {
  override def fnName: String = value.name

  def handler: ServiceHandler = _ => js.Promise.resolve(arg)
  def arrow: ArrowTypeDef = ArrowTypeDef(NilTypeDef, UnlabeledProductTypeDef(TopTypeDef :: Nil))
}

object ArgumentGetter {

  val ServiceId = "getDataSrv"

  private def getFunction(value: VarRaw, arg: scalajs.js.Dynamic) = GetFunction(value, arg)

  def apply(value: VarRaw, arg: scalajs.js.Dynamic): ArgumentGetter =
    new ArgumentGetter(ServiceId, getFunction(value, arg)) {

      override def callTag(): CallArrowRawTag =
        CallArrowRawTag.service(
          LiteralRaw.quote(ServiceId),
          value.name,
          Call(List.empty, List(Call.Export(value.name, value.baseType)))
        )
    }
}
