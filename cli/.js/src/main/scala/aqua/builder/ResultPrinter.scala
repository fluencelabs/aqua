package aqua.builder

import aqua.backend.{ArgDefinition, PrimitiveType, ServiceDef, ServiceFunctionDef, VoidType}
import aqua.io.OutputPrinter
import aqua.js.{CallJsFunction, CallServiceHandler, FluencePeer}
import aqua.raw.ops.{Call, CallServiceTag}
import aqua.raw.value.{LiteralRaw, VarRaw}

import scala.scalajs.js
import scala.scalajs.js.{Dynamic, JSON}

// Function to print any variables that passed as arguments
class ResultPrinter(serviceId: String, fnName: String, resultNames: List[String])
    extends ServiceFunction {

  def callTag(variables: List[VarRaw]): CallServiceTag =
    CallServiceTag(
      LiteralRaw.quote(serviceId),
      fnName,
      Call(variables, Nil)
    )

  def register(peer: FluencePeer): Unit = {
    CallJsFunction.registerService(
      peer,
      serviceId,
      (
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
        }
      ) :: Nil,
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
