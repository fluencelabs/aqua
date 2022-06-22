package aqua.builder

import aqua.backend.*
import aqua.js.{CallJsFunction, FluencePeer, ServiceHandler}
import cats.data.NonEmptyList
import scribe.Logging

import scala.scalajs.js
import scala.scalajs.js.{Dynamic, JSON}

class Service(serviceId: String, functions: NonEmptyList[AquaFunction]) extends Logging {

  def register(peer: FluencePeer): Unit = {
    val handlers = functions.map(f => (f.fnName, f.handler))
    val defs = LabeledProductTypeDef(
      functions.map(f => (f.fnName, f.arrow)).toList
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
        defs
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
