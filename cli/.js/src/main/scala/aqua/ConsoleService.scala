package aqua

import aqua.io.OutputPrinter
import aqua.js.{CallJsFunction, FluencePeer}
import aqua.model.func.Call
import aqua.model.{LiteralModel, VarModel}
import aqua.model.func.raw.CallServiceTag

import scala.scalajs.js.JSON

// Service to print any variables
class ConsoleService(serviceId: String, fnName: String) {

  def getCallServiceTag(variables: List[VarModel]): CallServiceTag = {
    CallServiceTag(
      LiteralModel.quote(serviceId),
      fnName,
      Call(variables, Nil)
    )
  }

  def registerService(peer: FluencePeer) = {
    CallJsFunction.registerUnitService(
      peer,
      serviceId,
      fnName,
      args => {
        val str = JSON.stringify(args, space = 2)
        // if an input function returns a result, our success will be after it is printed
        // otherwise finish after JS SDK will finish sending a request
        OutputPrinter.print(str)
      }
    )
  }
}
