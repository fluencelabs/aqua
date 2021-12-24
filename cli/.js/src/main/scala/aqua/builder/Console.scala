package aqua.builder

import aqua.backend.{ArgDefinition, PrimitiveType, ServiceDef, ServiceFunctionDef, VoidType}
import aqua.io.OutputPrinter
import aqua.js.{CallJsFunction, CallServiceHandler, FluencePeer}
import aqua.model.func.Call
import aqua.model.func.raw.CallServiceTag
import aqua.model.{LiteralModel, VarModel}

import scala.scalajs.js
import scala.scalajs.js.{Dynamic, JSON}

// Function to print any variables that passed as arguments
class Console(serviceId: String, fnName: String, resultNames: List[String])
    extends ServiceFunction {

  def callTag(variables: List[VarModel]): CallServiceTag = {
    CallServiceTag(
      LiteralModel.quote(serviceId),
      fnName,
      Call(variables, Nil)
    )
  }

  def registerService(peer: FluencePeer): Unit = {
    CallJsFunction.registerService(
      peer,
      serviceId,
      fnName,
      varArgs => {
        // drop last argument (tetraplets)
        val args: Seq[js.Any] = varArgs.init
        val toPrint = args.toList match {
          case arg :: Nil => JSON.stringify(arg, space = 2)
          case _ => args.map(a => JSON.stringify(a, space = 2)).mkString("[\n", ",\n", "\n]")
        }

        // if an input function returns a result, our success will be after it is printed
        // otherwise finish after JS SDK will finish sending a request
        OutputPrinter.print(toPrint)
        // empty JS object
        js.Promise.resolve(ServiceFunction.emptyObject)
      },
      ServiceDef(
        None,
        ServiceFunctionDef(
          fnName,
          resultNames.map(n => ArgDefinition(n, PrimitiveType)),
          VoidType
        ) :: Nil
      )
    )
  }
}
