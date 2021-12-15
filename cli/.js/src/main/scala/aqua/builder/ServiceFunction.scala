package aqua.builder

import aqua.js.{CallServiceHandler, FluencePeer}
import aqua.model.{LiteralModel, VarModel}
import aqua.model.func.Call
import aqua.model.func.raw.CallServiceTag

trait ServiceFunction {
  def registerService(peer: FluencePeer): CallServiceHandler
}

case class ServiceFunction2(fnName: String) {

  def call(
    serviceId: String,
    arguments: List[VarModel],
    exportTo: List[Call.Export]
  ): CallServiceTag =
    CallServiceTag(
      LiteralModel.quote(serviceId),
      fnName,
      Call(arguments, exportTo)
    )
}

case class ServicePrototype(functions: Map[String, ServiceFunction2])

case class Service(id: String, prototype: ServicePrototype) {
  def callFn(fn: String): CallServiceTag = ???
  def register(peer: FluencePeer): CallServiceHandler = ???
}
