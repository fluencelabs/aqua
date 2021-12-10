package aqua.builder

import aqua.io.OutputPrinter
import aqua.js.{CallJsFunction, CallServiceHandler, FluencePeer}
import aqua.model.func.Call
import aqua.model.func.raw.CallServiceTag
import aqua.model.{LiteralModel, VarModel}

import scala.scalajs.js.JSON

// Function to print any variables that passed as arguments
class Console(serviceId: String, fnName: String) extends ServiceFunction {

  def callTag(variables: List[VarModel]): CallServiceTag = {
    CallServiceTag(
      LiteralModel.quote(serviceId),
      fnName,
      Call(variables, Nil)
    )
  }

  def registerService(peer: FluencePeer): CallServiceHandler = {
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
