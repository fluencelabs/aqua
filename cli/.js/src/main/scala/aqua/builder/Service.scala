package aqua.builder

import aqua.backend.*
import aqua.io.OutputPrinter
import aqua.js.{CallJsFunction, CallServiceHandler, FluencePeer, ServiceHandler}
import cats.data.NonEmptyList
import scribe.Logging

import scala.scalajs.js
import scala.scalajs.js.{Dynamic, JSON}

class Service(serviceId: String, functions: NonEmptyList[AquaFunction]) extends Logging {

  def register(peer: FluencePeer): Unit = {
    val handlers = functions.map(f => (f.fnName, f.handler))
    val defs = LabelledProductTypeDef(
      functions.map(f => (f.fnName, ArrowTypeDef(f.argDefinitions, f.returnType)))
    )

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
  def arrow: ArrowTypeDef
}

object Service {
  val emptyObject: Dynamic = Dynamic.literal()
}
