package aqua.builder

import aqua.backend.{
  ArgDefinition,
  PrimitiveType,
  ServiceDef,
  ServiceFunctionDef,
  TypeDefinition,
  VoidType
}
import aqua.io.OutputPrinter
import aqua.js.{CallJsFunction, CallServiceHandler, FluencePeer, ServiceHandler}

import scalajs.js.{Dynamic, JSON}
import cats.data.NonEmptyList
import scribe.Logging

import scala.scalajs.js

class Service(serviceId: String, functions: NonEmptyList[AquaFunction]) extends Logging {

  def register(peer: FluencePeer): Unit = {
    val handlers = functions.map(f => (f.fnName, f.handler))
    val defs = functions.map(f => ServiceFunctionDef(f.fnName, f.argDefinitions, f.returnType))

    logger.debug(
      s"Registering service $serviceId with functions ${functions.map(_.fnName).toList.mkString(",")}"
    )

    CallJsFunction.registerService(
      peer,
      serviceId,
      handlers.toList,
      ServiceDef(
        None,
        defs.toList
      )
    )
  }
}

trait AquaFunction {
  def fnName: String
  def handler: ServiceHandler
  def argDefinitions: List[ArgDefinition]
  def returnType: TypeDefinition
}

object Service {
  val emptyObject: Dynamic = Dynamic.literal()
}
